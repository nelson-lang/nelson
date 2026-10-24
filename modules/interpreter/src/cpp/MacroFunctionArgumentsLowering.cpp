//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <string>
#include <vector>
#include "MacroFunctionArgumentsLowering.hpp"
#include "ArgumentsBlockLowering.hpp"
#include "ErrorID.hpp"
#include "LexerContext.hpp"
#include "ParserInterface.hpp"
#include "i18n.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
parseGeneratedStatements(
    MacroFunctionDef* self, const std::string& code, AbstractSyntaxTreePtr& head);
static AbstractSyntaxTreePtr
invalidArgumentsBlockStatement(MacroFunctionDef* self);
//=============================================================================
// Lower any leading "arguments" validation blocks captured natively by the
// grammar into plain validation statements prepended to the function body,
// and apply the varargin signature patch. Validation errors are reported at
// call time.
void
applyArgumentsBlockLowering(MacroFunctionDef* self)
{
    AbstractSyntaxTreePtr body = self->code;
    if (body == nullptr || body->opNum != OP_BLOCK || body->down == nullptr) {
        return;
    }
    // Nothing to lower unless the first non-empty statement is an arguments block.
    bool hasArgumentsBlock = false;
    for (AbstractSyntaxTreePtr stmt = body->down; stmt != nullptr; stmt = stmt->right) {
        if (stmt->down == nullptr || stmt->down->isEmpty()) {
            continue;
        }
        hasArgumentsBlock = (stmt->down->opNum == OP_ARGUMENTS);
        break;
    }
    if (!hasArgumentsBlock) {
        return;
    }
    // Gather the leading arguments blocks (interleaved blank statements are
    // preserved), rebuilding the body statement chain without them.
    std::vector<std::string> blockTexts;
    AbstractSyntaxTreePtr keptHead = nullptr;
    AbstractSyntaxTreePtr keptTail = nullptr;
    bool inLeading = true;
    for (AbstractSyntaxTreePtr stmt = body->down; stmt != nullptr;) {
        AbstractSyntaxTreePtr next = stmt->right;
        const bool isArgumentsStmt
            = inLeading && stmt->down != nullptr && stmt->down->opNum == OP_ARGUMENTS;
        if (isArgumentsStmt) {
            AbstractSyntaxTreePtr textNode = stmt->down->down;
            blockTexts.push_back(textNode != nullptr ? textNode->text : std::string());
        } else {
            if (stmt->down != nullptr && !stmt->down->isEmpty()) {
                inLeading = false;
            }
            stmt->right = nullptr;
            if (keptHead == nullptr) {
                keptHead = stmt;
            } else {
                keptTail->right = stmt;
            }
            keptTail = stmt;
        }
        stmt = next;
    }
    body->down = keptHead;

    ArgumentsLoweringResult lowered = lowerArgumentsBlocks(blockTexts, self->arguments);
    if (lowered.paramsPatched) {
        self->arguments = lowered.patchedParams;
    }
    // Prologue (input/structural validation) runs at the top of the body and
    // epilogue (output validation) at its end. Generated code that does not
    // parse fails fast: the validation is never skipped silently.
    AbstractSyntaxTreePtr prologueHead = nullptr;
    AbstractSyntaxTreePtr epilogueHead = nullptr;
    if (!parseGeneratedStatements(self, lowered.generatedCode, prologueHead)
        || !parseGeneratedStatements(self, lowered.outputValidationCode, epilogueHead)) {
        prologueHead = invalidArgumentsBlockStatement(self);
        epilogueHead = nullptr;
    }
    if (prologueHead != nullptr) {
        AbstractSyntaxTreePtr prologueTail = prologueHead;
        while (prologueTail->right != nullptr) {
            prologueTail = prologueTail->right;
        }
        prologueTail->right = body->down;
        body->down = prologueHead;
    }
    // Epilogue (output validation) runs at the end of the body, after the
    // outputs have been assigned but before the trailing function-terminator
    // return (the compiled form of the closing 'end'), so it is not skipped.
    if (epilogueHead != nullptr) {
        AbstractSyntaxTreePtr epilogueTail = epilogueHead;
        while (epilogueTail->right != nullptr) {
            epilogueTail = epilogueTail->right;
        }
        if (body->down == nullptr) {
            body->down = epilogueHead;
        } else {
            AbstractSyntaxTreePtr last = body->down;
            AbstractSyntaxTreePtr prev = nullptr;
            while (last->right != nullptr) {
                prev = last;
                last = last->right;
            }
            const bool lastIsTerminator = last->down != nullptr && last->down->type == reserved_node
                && static_cast<unsigned int>(last->down->tokenNumber) == NLS_KEYWORD_RETURN;
            if (lastIsTerminator) {
                epilogueTail->right = last;
                if (prev == nullptr) {
                    body->down = epilogueHead;
                } else {
                    prev->right = epilogueHead;
                }
            } else {
                last->right = epilogueHead;
            }
        }
    }
}
//=============================================================================
// Parse a snippet of generated validation statements and transfer node
// ownership to 'self'. Returns false when the snippet does not parse;
// otherwise 'head' is the parsed statement chain (nullptr when empty).
static bool
parseGeneratedStatements(
    MacroFunctionDef* self, const std::string& code, AbstractSyntaxTreePtr& head)
{
    head = nullptr;
    if (code.empty()) {
        return true;
    }
    AbstractSyntaxTree::clearReferences();
    LexerContext lexerContext;
    ParserContext parsed = parseStringResult(lexerContext, code);
    if (parsed.state() != ScriptBlock) {
        AbstractSyntaxTree::clearReferences();
        return false;
    }
    if (parsed.ast == nullptr || parsed.ast->down == nullptr) {
        AbstractSyntaxTree::clearReferences();
        return true;
    }
    AbstractSyntaxTreePtrVector generatedRefs = AbstractSyntaxTree::getReferences();
    for (auto* node : generatedRefs) {
        if (node != nullptr) {
            self->ptrAstCodeAsVector.push_back(node);
        }
    }
    AbstractSyntaxTree::clearReferences();
    head = parsed.ast->down;
    return true;
}
//=============================================================================
// The validation code generated from the arguments blocks does not parse
// (malformed entry: default value, validator, ...). Running the function
// without its validation would silently return wrong results, so its body
// becomes a call-time error naming the function instead.
static AbstractSyntaxTreePtr
invalidArgumentsBlockStatement(MacroFunctionDef* self)
{
    const std::string id = "Nelson:interpreter:invalidArgumentsBlock";
    std::string message = formatI18nMessage(
        resolveMessageTemplate(id, "Invalid arguments block in function '%s'."), self->getName());
    std::string escaped;
    for (char c : message) {
        escaped += c;
        if (c == '\'') {
            escaped += c;
        }
    }
    AbstractSyntaxTreePtr head = nullptr;
    if (!parseGeneratedStatements(self, "error('" + id + "', '" + escaped + "');", head)
        || head == nullptr) {
        RaiseError(message, id);
    }
    return head;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

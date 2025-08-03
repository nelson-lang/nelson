//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif
//=============================================================================
#define FMT_HEADER_ONLY
#include <fmt/core.h>
#include "i18n.hpp"
#include "Evaluator.hpp"
#include "Error.hpp"
#include "MacroFunctionDef.hpp"
#include "Warning.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Returns the current debug depth.
int
Evaluator::getDebugDepth()
{
    return depth;
}
//=============================================================================
// Increases the debug depth by one.
void
Evaluator::increaseDebugDepth()
{
    depth++;
}
//=============================================================================
// Decreases the debug depth by one.
void
Evaluator::decreaseDebugDepth()
{
    depth--;
}
//=============================================================================
// Handles debug events, including breakpoints and step mode.
void
Evaluator::handleDebug(int fullcontext)
{
    if (debugActive) {
        int linenumber = fullcontext & 0xffff;
        if (inStepMode) {
            if ((stepTrap.cname == callstack.getLastContext()) && (stepTrap.tokid == linenumber)) {
                // Finished stepping...
                inStepMode = false;
                std::string message = fmt::format(
                    _("Finished stepping to {}, line {}\n"), stepTrap.cname, linenumber);
                io->outputMessage(message);
                debugCLI();
            }
        } else {
            // check the breakpoint list
            bool found = false;
            size_t j = 0;
            while ((j < bpStack.size()) && !found) {
                // Is this a resolved breakpoint?
                if ((bpStack[j].tokid >> 16) != 0) {
                    if ((bpStack[j].cname == callstack.getLastContext())
                        && (bpStack[j].tokid == fullcontext)) {
                        found = true;
                    } else {
                        found = false;
                        j++;
                    }
                } else {
                    if ((bpStack[j].cname == callstack.getLastContext())
                        && (bpStack[j].tokid == linenumber)) {
                        found = true;
                        bpStack[j].tokid = fullcontext;
                    } else {
                        found = false;
                        j++;
                    }
                }
            }
            if (found) {
                stepTrap = bpStack[j];
                std::string message = fmt::format(
                    _("Encountered breakpoint at {}, line {}\n"), bpStack[j].cname, linenumber);
                io->outputMessage(message);
                debugCLI();
            }
        }
    }
}
//=============================================================================
// Returns whether autostop is enabled.
bool
Evaluator::AutoStop()
{
    return autostop;
}
//=============================================================================
// Sets the autostop flag.
void
Evaluator::AutoStop(bool a)
{
    autostop = a;
}
//=============================================================================
// Enters the debug command line interface.
void
Evaluator::debugCLI()
{
    depth++;
    bpActive = true;
    evalCLI();
    bpActive = false;
    if (state == NLS_STATE_RETURN) {
        resetState();
    }
    depth--;
}
//=============================================================================
// Adds a breakpoint to the stack and adjusts breakpoints.
void
Evaluator::addBreakpoint(StackEntry& bp)
{
    bpStack.push_back(bp);
    adjustBreakpoints();
    debugActive = true;
}
//=============================================================================
// Lists all breakpoints to the output.
void
Evaluator::listBreakpoints()
{
    for (size_t i = 0; i < bpStack.size(); i++) {
        std::string message = fmt::format(
            _("{}   {} line {}\n"), i + 1, bpStack[i].cname, bpStack[i].tokid & 0xffff);
        io->outputMessage(message);
    }
}
//=============================================================================
// Deletes a breakpoint by its number.
void
Evaluator::deleteBreakpoint(int number)
{
    if ((number >= 1) && (number <= (int)bpStack.size())) {
        bpStack.erase(bpStack.begin() + number - 1);
    } else {
        std::string message = fmt::format(_("Unable to delete breakpoint {}"), number);
        Error(message);
    }
}
//=============================================================================
// Calculates the cost (distance) between two line numbers.
static int
COST(int a, int b)
{
    return (((a) >= (b)) ? ((a) - (b)) : 10000);
}
//=============================================================================
// Finds the closest line number in the syntax tree to the given line.
static int
GetClosestLineNumber(AbstractSyntaxTreePtr t, int lineno)
{
    if (t == nullptr) {
        return 10000;
    }
    int linedwn = GetClosestLineNumber(t->down, lineno);
    int linerght = GetClosestLineNumber(t->right, lineno);
    int retval = (t->getContext() & 0xffff);
    int costthis = COST(retval, lineno);
    return (std::min(linedwn, std::min(linerght, costthis)));
}
//=============================================================================
// Adjusts a breakpoint to the closest valid line in a macro function.
bool
Evaluator::adjustBreakpoint(StackEntry& bp, bool dbstep)
{
    bool isFun;
    FunctionDefPtr val;
    std::string cname = bp.detail;
    isFun = context->lookupFunction(cname, val);
    if (!isFun) {
        return false;
    }
    if (val->type() == NLS_MACRO_FUNCTION) {
        MacroFunctionDef* mptr;
        mptr = (MacroFunctionDef*)val;
        int clinenum = 10000;
        while (mptr != nullptr) {
            AbstractSyntaxTreePtr code = mptr->code;
            int nxt = GetClosestLineNumber(code, bp.tokid & 0xffff);
            clinenum = std::min(clinenum, nxt);
            mptr = mptr->nextFunction;
        }
        if (clinenum == 10000) {
            std::string message;
            if (dbstep) {
                message = fmt::format("{}",
                    _("Unable to step the specified number of lines, execution will continue\n"));
                inStepMode = false;
            } else {
                message = fmt::format(
                    _("Failed to set breakpoint in {} at line {} - breakpoint is disabled\n"),
                    cname, bp.tokid & 0xffff);
            }
            Warning(message);
            return false;
        }
        if (clinenum != 0) {
            bp.tokid = (bp.tokid & 0xffff) + clinenum;
        }
    } else {
        return false;
    }
    return true;
}
//=============================================================================
// Adjusts all breakpoints in the stack.
void
Evaluator::adjustBreakpoints()
{
    std::vector<StackEntry>::iterator i = bpStack.begin();
    while (i != bpStack.end()) {
        if (!adjustBreakpoint(*i, false)) {
            std::vector<StackEntry>::iterator b = i;
            bpStack.erase(b);
            ++i;
        } else {
            ++i;
        }
    }
    if (inStepMode) {
        adjustBreakpoint(stepTrap, true);
    }
}
//=============================================================================
// Steps the debugger by a given number of lines.
void
Evaluator::dbstep(int linecount)
{
    if (bpActive) {
        stepTrap.tokid = (stepTrap.tokid & 0xffff) + linecount;
        inStepMode = true;
        adjustBreakpoints();
        state = NLS_STATE_RETURN;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================

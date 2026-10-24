//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "AnonymousMacroFunctionDef.hpp"
#include "FunctionHandleOwner.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
deleteAnonymousMacroFunctionDef(nelson_handle* anonymousHandle);
//=============================================================================
namespace {
    //=============================================================================
    // Definitions released while another one is being destroyed on this thread
    // (a handle capturing a handle capturing a handle ...). They are destroyed
    // by the outermost deletion, one after the other, so that a long chain of
    // captures never recurses.
    struct DeletionQueue
    {
        std::vector<AnonymousMacroFunctionDef*> pending;
        bool draining = false;
    };
    //=============================================================================
    DeletionQueue&
    threadDeletionQueue()
    {
        static thread_local DeletionQueue queue;
        return queue;
    }
    //=============================================================================
    // Function handle values own their definition through FunctionHandleOwner
    // (types module), which destroys it with the deleter registered here. The
    // registration lasts while this module is loaded: a value released after
    // the module unloaded leaves its definition alive instead of calling
    // unloaded code.
    struct DefinitionDeleterRegistration
    {
        DefinitionDeleterRegistration()
        {
            setFunctionHandleDefinitionDeleter(deleteAnonymousMacroFunctionDef);
        }
        ~DefinitionDeleterRegistration() { setFunctionHandleDefinitionDeleter(nullptr); }
    };
    //=============================================================================
    const DefinitionDeleterRegistration definitionDeleterRegistration;
    //=============================================================================
} // namespace
//=============================================================================
static void
deleteAnonymousMacroFunctionDef(nelson_handle* anonymousHandle)
{
    auto* definition = reinterpret_cast<AnonymousMacroFunctionDef*>(anonymousHandle);
    DeletionQueue& queue = threadDeletionQueue();
    if (queue.draining) {
        queue.pending.push_back(definition);
        return;
    }
    queue.draining = true;
    delete definition;
    while (!queue.pending.empty()) {
        AnonymousMacroFunctionDef* next = queue.pending.back();
        queue.pending.pop_back();
        delete next;
    }
    queue.draining = false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

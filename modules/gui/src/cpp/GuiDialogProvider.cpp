//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GuiDialogProvider.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static GuiDialogProviderFn s_provider;
//=============================================================================
void
setGuiDialogProvider(GuiDialogProviderFn provider)
{
    s_provider = std::move(provider);
}
//=============================================================================
bool
hasGuiDialogProvider()
{
    return static_cast<bool>(s_provider);
}
//=============================================================================
bool
runGuiDialog(const GuiDialogRequest& request, GuiDialogResponse& response)
{
    if (!s_provider) {
        return false;
    }
    return s_provider(request, response);
}
//=============================================================================
extern "C" NLSGUI_IMPEXP void
nlsGuiRegisterDialogProvider(GuiDialogProviderRawFn provider, void* context)
{
    if (provider == nullptr) {
        s_provider = nullptr;
        return;
    }
    s_provider = [provider, context](const GuiDialogRequest& request, GuiDialogResponse& response) {
        return provider(context, request, response);
    };
}
//=============================================================================
} // namespace Nelson
//=============================================================================

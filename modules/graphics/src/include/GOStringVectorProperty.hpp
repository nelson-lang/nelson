//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "nlsGraphics_exports.h"
//=============================================================================
#include <vector>
#include "GOGenericProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOStringVectorProperty : public GOGenericProperty
{
protected:
    std::vector<std::wstring> _data;
    bool _returnAsCell = false;
    bool _returnAsStringRow = false;
    bool _returnAsRowCell = false;
    bool _pipeDelimited = true;
    bool _emptyReturnsChar = false;

public:
    void
    setEmptyReturnsChar(bool value)
    {
        _emptyReturnsChar = value;
    }
    GOStringVectorProperty() = default;
    GOStringVectorProperty(bool returnAsCell) : _returnAsCell(returnAsCell) { }
    GOStringVectorProperty(bool returnAsCell, bool returnAsStringRow)
        : _returnAsCell(returnAsCell), _returnAsStringRow(returnAsStringRow)
    {
    }
    GOStringVectorProperty(bool returnAsCell, bool returnAsStringRow, bool returnAsRowCell)
        : _returnAsCell(returnAsCell)
        , _returnAsStringRow(returnAsStringRow)
        , _returnAsRowCell(returnAsRowCell)
    {
    }
    ~GOStringVectorProperty() override = default;
    ArrayOf
    get() override;
    void set(ArrayOf) override;
    std::vector<std::wstring>
    data();
    void
    data(const std::wstring& m);
    void
    data(const std::vector<std::wstring>& m);
    std::wstring
    toWideString() override;
};
//=============================================================================
// The text of UI components: a '|' in a character row is a literal character,
// not the entry separator of the legacy uicontrol String.
class GOUITextVectorProperty : public GOStringVectorProperty
{
public:
    GOUITextVectorProperty() { _pipeDelimited = false; }
    GOUITextVectorProperty(bool returnAsCell) : GOStringVectorProperty(returnAsCell)
    {
        _pipeDelimited = false;
    }
    GOUITextVectorProperty(bool returnAsCell, bool returnAsStringRow, bool returnAsRowCell)
        : GOStringVectorProperty(returnAsCell, returnAsStringRow, returnAsRowCell)
    {
        _pipeDelimited = false;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================

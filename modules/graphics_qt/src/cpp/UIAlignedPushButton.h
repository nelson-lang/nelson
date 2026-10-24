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
#include <string>
#include <QtWidgets/QPushButton>
//=============================================================================
namespace Nelson {
//=============================================================================
// QPushButton always centers its label vertically. This button honours the
// VerticalAlignment property ('top' | 'center' | 'bottom') of uibutton, the
// state button and uitogglebutton: the face is drawn over the whole widget,
// the label (text and icon, horizontal alignment unchanged) in a band at the
// top or at the bottom.
//=============================================================================
class UIAlignedPushButton : public QPushButton
{
public:
    explicit UIAlignedPushButton(QWidget* parent);

    void
    setVerticalAlignment(const std::wstring& alignment);

protected:
    void
    paintEvent(QPaintEvent* event) override;

private:
    Qt::Alignment verticalAlignment = Qt::AlignVCenter;
};
//=============================================================================
} // namespace Nelson
//=============================================================================

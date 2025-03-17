//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <QtGui/QImage>
#include <QtCore/QString>
#include <QtCore/QVector>
#include <QtGui/QRgb>
#include <QtCore/QFile>
//=============================================================================
class PcxFileHandler
{
    //=============================================================================
public:
    //=============================================================================
    // PCX error codes
    enum PCXErrorCode
    {
        PCX_ERROR_NONE = 0,
        PCX_ERROR_OPENING,
        PCX_ERROR_READING,
        PCX_ERROR_WRITING,
        PCX_ERROR_NO_HEADER,
        PCX_ERROR_WRONG_VERSION,
        PCX_ERROR_NO_PALETTE,
        PCX_ERROR_MEMORY
    };
    //=============================================================================
    // Load a PCX file into a QImage
    static QImage
    loadPCX(const QString& filePath);
    //=============================================================================
    // Save a QImage as a PCX file
    static int
    savePCX(const QString& filePath, const QImage& image);
    //=============================================================================
private:
    static int
    pcxEncodeByte(QFile& file, quint8 byte, quint8 count);
    static int
    pcxEncodeLine(QFile& file, const QVector<quint8>& data, int length);
};
//=============================================================================

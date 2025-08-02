//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include "GetFrame.hpp"
#include "GOWindow.hpp"
#include "GOAxis.hpp"
#include "BaseFigureQt.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static ArrayOf
QImageToArrayOf(QImage image);
//=============================================================================
ArrayOf
GetFrame(GOWindow* f)
{

    if (!f) {
        return ArrayOf();
    }
    QWidget* w = f->getMainQWigdet();
    if (!w) {
        return ArrayOf();
    }
    QImage image = ((BaseFigureQt*)w)->getFrame();
    return QImageToArrayOf(image);
}
//=============================================================================
ArrayOf
QImageToArrayOf(QImage image)
{
    int width = image.width();
    int height = image.height();
    std::vector<indexType> dimsAsVector;
    dimsAsVector.reserve(3);
    dimsAsVector.push_back(height);
    dimsAsVector.push_back(width);
    dimsAsVector.push_back(3);
    Dimensions dimsA(dimsAsVector);
    uint8* ptrA = (uint8*)ArrayOf::allocateArrayOf(NLS_UINT8, dimsA.getElementCount());
    ArrayOf A = ArrayOf(NLS_UINT8, dimsA, ptrA);

    int nbElements = height * width;
    OMP_PARALLEL_FOR_LOOP(nbElements)
    for (int index = 0; index < nbElements; index++) {
        int row = index % height;
        int col = index / height;
        const QRgb* scanLine = reinterpret_cast<const QRgb*>(image.constScanLine(row));
        QRgb pixel = scanLine[col];
        ptrA[index] = qRed(pixel);
        ptrA[index + height * width] = qGreen(pixel);
        ptrA[index + 2 * height * width] = qBlue(pixel);
    }
    return A;
}
//=============================================================================
}
//=============================================================================

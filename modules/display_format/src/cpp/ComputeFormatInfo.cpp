//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include "ComputeFormatInfo.hpp"
#include "IEEEFP.hpp"
#include "FormatTemplateHelpers.hpp"
#include "ScaleFactor.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static FormatDisplayInformation
getArrayOfFormatInfo(bool asSingle, bool asComplex, NumericFormatDisplay currentNumericFormat);
//=============================================================================
static FormatDisplayInformation
getArrayOfFormatInfoSingle(NumericFormatDisplay currentNumericFormat);
//=============================================================================
static FormatDisplayInformation
getArrayOfFormatInfoSingleComplex(NumericFormatDisplay currentNumericFormat);
//=============================================================================
static FormatDisplayInformation
computeFormatInfo(
    double realPart, double imagPart, bool asSingle, NumericFormatDisplay currentNumericFormat);
//=============================================================================
static FormatDisplayInformation
computeFormatInfo(double val, bool asSingle, NumericFormatDisplay currentNumericFormat);
//=============================================================================
static bool
computeWidthRealByScaleFactor(FormatDisplayInformation& formatInfo)
{
    if (formatInfo.scaleFactor < 9) {
        formatInfo.formatReal = L"%*ld";
        formatInfo.floatAsInteger = true;
        int scaleFactor = (int)formatInfo.scaleFactor;
        switch (scaleFactor) {
        case 1:
        case 2: {
            formatInfo.widthReal = 6;
        } break;
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
        case 8:
        default: {
            formatInfo.widthReal = 12;
        } break;
        }
        formatInfo.decimalsReal = 0;
        return true;
    }
    return false;
}
//=============================================================================
FormatDisplayInformation
computeFormatInfo(const ArrayOf& A, NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    if (A.getNonzeros() == 1) {
        bool asSingle = false;
        if (A.isComplex()) {
            double realPart;
            double imagPart;
            if (A.isSingleClass()) {
                single* pValue = (single*)A.getDataPointer();
                realPart = (double)pValue[0];
                imagPart = (double)pValue[1];
                asSingle = true;
            } else {
                if (A.isSparse()) {
                    Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat
                        = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)
                              A.getSparseDataPointer();

                    const std::complex<double>* values = spMat->valuePtr();
                    realPart = values[0].real();
                    imagPart = values[0].imag();
                } else {
                    double* pValue = (double*)A.getDataPointer();
                    realPart = pValue[0];
                    imagPart = pValue[1];
                }
                asSingle = false;
            }
            if (asSingle && imagPart == 0.) {
                formatInfo = computeFormatInfo(realPart, asSingle, currentNumericFormat);
            } else {
                formatInfo = computeFormatInfo(realPart, imagPart, asSingle, currentNumericFormat);
            }
        } else {
            double value;
            if (A.isSingleClass()) {
                single* pValue = (single*)A.getDataPointer();
                value = (double)pValue[0];
                asSingle = true;
            } else {
                if (A.isSparse()) {
                    Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                        = (Eigen::SparseMatrix<double, 0, signedIndexType>*)
                              A.getSparseDataPointer();
                    const double* values = spMat->valuePtr();
                    value = values[0];
                } else {
                    double* pValue = (double*)A.getDataPointer();
                    value = pValue[0];
                }
                asSingle = false;
            }
            formatInfo = computeFormatInfo(value, asSingle, currentNumericFormat);
        }
        return formatInfo;
    }

    bool isFinite = false;
    bool allInteger = false;
    double absoluteValue;
    formatInfo = getArrayOfFormatInfo(A.isSingleClass(), A.isComplex(), currentNumericFormat);

    if (A.isSingleClass()) {
        single minValue = 0;
        single maxValue = 0;
        if (A.isSparse()) {
            if (A.isComplex()) {
                Eigen::SparseMatrix<std::complex<single>, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<std::complex<single>, 0, signedIndexType>*)
                          A.getSparseDataPointer();
                std::complex<single>* zdata = spMat->valuePtr();
                const single* data = reinterpret_cast<single*>(zdata);
                allInteger = IsIntegerFormOrNotFinite(data, spMat->nonZeros() * 2);
                getFiniteMinMax<single>(data, spMat->nonZeros() * 2, minValue, maxValue, isFinite);
            } else {
                Eigen::SparseMatrix<single, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<single, 0, signedIndexType>*)A.getSparseDataPointer();
                const single* data = spMat->valuePtr();
                allInteger = IsIntegerFormOrNotFinite(data, spMat->nonZeros());
                getFiniteMinMax<single>(data, spMat->nonZeros(), minValue, maxValue, isFinite);
            }
        } else {
            if (A.isComplex()) {
                const single* pValues = (const single*)A.getDataPointer();
                allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount() * 2);
                getFiniteMinMax<single>(
                    pValues, A.getElementCount() * 2, minValue, maxValue, isFinite);
            } else {
                const single* pValues = (const single*)A.getDataPointer();
                allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount());
                getFiniteMinMax<single>(pValues, A.getElementCount(), minValue, maxValue, isFinite);
            }
        }
        absoluteValue = std::max(fabs(maxValue), fabs(minValue));

    } else {
        double minValue = 0;
        double maxValue = 0;
        if (A.isSparse()) {
            if (A.isComplex()) {
                Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<std::complex<double>, 0, signedIndexType>*)
                          A.getSparseDataPointer();
                std::complex<double>* zdata = spMat->valuePtr();
                const double* data = reinterpret_cast<double*>(zdata);
                allInteger = IsIntegerFormOrNotFinite(data, spMat->nonZeros() * 2);
                getFiniteMinMax<double>(data, spMat->nonZeros() * 2, minValue, maxValue, isFinite);
            } else {
                Eigen::SparseMatrix<double, 0, signedIndexType>* spMat
                    = (Eigen::SparseMatrix<double, 0, signedIndexType>*)A.getSparseDataPointer();
                const double* data = spMat->valuePtr();
                allInteger = IsIntegerFormOrNotFinite(data, spMat->nonZeros());
                getFiniteMinMax<double>(data, spMat->nonZeros(), minValue, maxValue, isFinite);
            }
        } else {
            if (A.isComplex()) {
                const double* pValues = (const double*)A.getDataPointer();
                allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount() * 2);
                getFiniteMinMax<double>(
                    pValues, A.getElementCount() * 2, minValue, maxValue, isFinite);
            } else {
                const double* pValues = (const double*)A.getDataPointer();
                allInteger = IsIntegerFormOrNotFinite(pValues, A.getElementCount());
                getFiniteMinMax<double>(pValues, A.getElementCount(), minValue, maxValue, isFinite);
            }
        }
        absoluteValue = (double)std::max(fabs(maxValue), fabs(minValue));
    }
    if (A.isComplex()) {
        switch (currentNumericFormat) {
        case NLS_NUMERIC_FORMAT_LONG: {
            ComputeScaleFactor(A, allInteger, formatInfo);
            formatInfo.isComplex = true;
            if (A.isSingleClass()) {
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 11;
                formatInfo.decimalsReal = 7;

                formatInfo.formatImag = L"%*.*f";
                formatInfo.widthImag = 10;
                formatInfo.decimalsImag = 7;

            } else {
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 19;
                formatInfo.decimalsReal = 15;

                formatInfo.formatImag = L"%*.*f";
                formatInfo.widthImag = 18;
                formatInfo.decimalsImag = 15;
            }

        } break;
        case NLS_NUMERIC_FORMAT_SHORT: {
            ComputeScaleFactor(A, allInteger, formatInfo);
            formatInfo.isComplex = true;
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 9;
            formatInfo.decimalsReal = 4;

            formatInfo.formatImag = L"%*.*f";
            formatInfo.widthImag = 7;
            formatInfo.decimalsImag = 4;
        } break;
        default: {
        } break;
        }
    } else {
        if (allInteger) {
            switch (currentNumericFormat) {
            case NLS_NUMERIC_FORMAT_SHORT: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.floatAsInteger = false;
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 10;
                formatInfo.decimalsReal = 4;
                if (computeWidthRealByScaleFactor(formatInfo)) {
                    formatInfo.scaleFactor = 1;
                };
            } break;
            case NLS_NUMERIC_FORMAT_LONG: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.floatAsInteger = false;
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 20;
                    formatInfo.decimalsReal = 15;
                }
                computeWidthRealByScaleFactor(formatInfo);
                if (computeWidthRealByScaleFactor(formatInfo)) {
                    formatInfo.scaleFactor = 1;
                };
            } break;
            case NLS_NUMERIC_FORMAT_SHORTE: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.floatAsInteger = false;
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 4;
                } else {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 13;
                    formatInfo.decimalsReal = 4;
                }
                computeWidthRealByScaleFactor(formatInfo);
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_LONGE: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.floatAsInteger = false;
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
                computeWidthRealByScaleFactor(formatInfo);
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_SHORTG: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.floatAsInteger = false;
                formatInfo.formatReal = L"%*.*g";
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 5;
                computeWidthRealByScaleFactor(formatInfo);
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_LONGG: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.floatAsInteger = false;
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*g";
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*g";
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
                computeWidthRealByScaleFactor(formatInfo);
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_RATIONAL: {
                formatInfo.widthReal = 11;
                formatInfo.decimalsReal = 9;
                formatInfo.formatReal = L"%*s";
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_BANK: {
                formatInfo.widthReal = 14;
                formatInfo.widthImag = 14;
                formatInfo.formatReal = L"%*.*f";
                formatInfo.decimalsReal = 2;
                formatInfo.formatImag = L"%*.*f";
                formatInfo.decimalsImag = 2;
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_HEX: {
                formatInfo.scaleFactor = 1;
            } break;
            default: {
            } break;
            }
        } else {
            formatInfo.floatAsInteger = false;
            switch (currentNumericFormat) {
            case NLS_NUMERIC_FORMAT_LONGG: {
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*g";
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*g";
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
            } break;
            case NLS_NUMERIC_FORMAT_SHORTG: {
                formatInfo.formatReal = L"%*.*g";
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 5;
            } break;
            case NLS_NUMERIC_FORMAT_SHORTE: {
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 4;
                } else {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 13;
                    formatInfo.decimalsReal = 4;
                }
            } break;
            case NLS_NUMERIC_FORMAT_LONGE: {
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
            } break;
            case NLS_NUMERIC_FORMAT_LONG: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 20;
                    formatInfo.decimalsReal = 15;
                }
            } break;
            case NLS_NUMERIC_FORMAT_SHORT: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 10;
                formatInfo.decimalsReal = 4;
            } break;
            case NLS_NUMERIC_FORMAT_RATIONAL: {
                formatInfo.widthReal = 11;
                formatInfo.decimalsReal = 9;
                formatInfo.formatReal = L"%*s";
            } break;
            case NLS_NUMERIC_FORMAT_BANK: {
                formatInfo.widthReal = 14;
                formatInfo.widthImag = 14;
                formatInfo.formatReal = L"%*.*f";
                formatInfo.decimalsReal = 2;
                formatInfo.formatImag = L"%*.*f";
                formatInfo.decimalsImag = 2;
            } break;
            case NLS_NUMERIC_FORMAT_HEX: {
            } break;
            default: {
            } break;
            }
        }
    }
    return formatInfo;
}
//=============================================================================
static FormatDisplayInformation
getArrayOfFormatInfo(bool asSingle, bool asComplex, NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo;
    formatInfo.isComplex = asComplex;
    formatInfo.numericFormatDisplay = currentNumericFormat;
    formatInfo.floatAsInteger = false;
    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORT: {
        formatInfo.widthReal = 9;
        formatInfo.decimalsReal = 4;
        formatInfo.widthImag = 9;
        formatInfo.decimalsImag = 4;
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        if (asSingle) {
            formatInfo.widthReal = 18;
            formatInfo.floatAsInteger = false;
            formatInfo.decimalsReal = 7;
            formatInfo.widthImag = 18;
            formatInfo.decimalsImag = 7;
        } else {
            formatInfo.widthReal = 18;
            formatInfo.decimalsReal = 18;
            formatInfo.widthImag = 18;
            formatInfo.decimalsImag = 18;
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE: {
        if (asSingle) {
            formatInfo.widthReal = 12;
            formatInfo.decimalsReal = 4;
            formatInfo.formatReal = L"%*.*e";
            formatInfo.widthImag = 10;
            formatInfo.decimalsImag = 4;
            formatInfo.formatImag = L"%*.*e";
        } else {
            formatInfo.widthReal = 13;
            formatInfo.decimalsReal = 4;
            formatInfo.formatReal = L"%*.*e";
            formatInfo.widthImag = 11;
            formatInfo.decimalsImag = 4;
            formatInfo.formatImag = L"%*.*e";
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
        if (asSingle) {
            if (asComplex) {
                formatInfo.widthReal = 15;
                formatInfo.decimalsReal = 7;
                formatInfo.formatReal = L"%*.*e";
                formatInfo.widthImag = 14;
                formatInfo.decimalsImag = 7;
                formatInfo.formatImag = L"%*.*e";
            } else {
                formatInfo.widthReal = 16;
                formatInfo.formatReal = L"%*.*e";
                formatInfo.decimalsReal = 7;
            }
        } else {
            formatInfo.formatReal = L"%*.*e";
            formatInfo.decimalsReal = 15;
            if (asComplex) {
                formatInfo.widthReal = 27;
                formatInfo.widthImag = 22;
                formatInfo.decimalsImag = 15;
                formatInfo.formatImag = L"%*.*e";
            } else {
                formatInfo.widthReal = 26;
                formatInfo.formatReal = L"%*.*e";
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
        if (asSingle) {
            if (asComplex) {
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 5;
                formatInfo.formatReal = L"%*.*g";
                formatInfo.widthImag = 11;
                formatInfo.decimalsImag = 5;
                formatInfo.formatImag = L"%*.*g";
            } else {
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 5;
                formatInfo.formatReal = L"%*.*g";
            }
        } else {
            formatInfo.widthReal = 13;
            formatInfo.decimalsReal = 5;
            formatInfo.formatReal = L"%*.*g";
            if (asComplex) {
                formatInfo.widthImag = 11;
                formatInfo.decimalsImag = 5;
                formatInfo.formatImag = L"%*.*g";
            } else {
                formatInfo.widthImag = 0;
                formatInfo.decimalsImag = 0;
                formatInfo.formatImag = L"%*.*g";
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
        if (asSingle) {
            if (asComplex) {
                formatInfo.widthReal = 14;
                formatInfo.decimalsReal = 7;
                formatInfo.formatReal = L"%*.*g";
                formatInfo.widthImag = 13;
                formatInfo.decimalsImag = 7;
                formatInfo.formatImag = L"%*.*g";
            } else {
                formatInfo.widthReal = 16;
                formatInfo.decimalsReal = 7;
                formatInfo.formatReal = L"%*.*g";
            }
        } else {
            if (asComplex) {
                formatInfo.widthReal = 27;
                formatInfo.decimalsReal = 15;
                formatInfo.formatReal = L"%*.*g";
                formatInfo.widthImag = 22;
                formatInfo.decimalsImag = 15;
                formatInfo.formatImag = L"%*.*g";
            } else {
                formatInfo.widthReal = 26;
                formatInfo.decimalsReal = 15;
                formatInfo.formatReal = L"%*.*g";
                formatInfo.widthImag = 0;
                formatInfo.decimalsImag = 0;
                formatInfo.formatImag = L"%*.*g";
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        if (asSingle) {
            if (asComplex) {
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 7;
                formatInfo.widthImag = 13;
                formatInfo.decimalsImag = 7;

            } else {
                formatInfo.widthReal = 17;
                formatInfo.decimalsReal = 16;
            }
        } else {
            if (asComplex) {
                formatInfo.widthReal = 17;
                formatInfo.decimalsReal = 0;
                formatInfo.widthImag = 13;
                formatInfo.decimalsImag = 0;
            } else {
                formatInfo.widthReal = 17;
                formatInfo.decimalsReal = 17;
                formatInfo.widthImag = 0;
                formatInfo.decimalsImag = 0;
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        if (asSingle) {
            if (asComplex) {
                formatInfo.widthReal = 34;
                formatInfo.decimalsReal = 0;
                formatInfo.widthImag = 34;
                formatInfo.decimalsImag = 0;
            } else {
                formatInfo.widthReal = 17;
                formatInfo.decimalsReal = 17;
            }
        } else {
            if (asComplex) {
                formatInfo.widthReal = 51;
                formatInfo.decimalsReal = 0;
                formatInfo.widthImag = 51;
                formatInfo.decimalsImag = 0;
            } else {
                formatInfo.widthReal = 26;
                formatInfo.decimalsReal = 26;
                formatInfo.widthImag = 0;
                formatInfo.decimalsImag = 0;
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        formatInfo.widthReal = 1;
        formatInfo.widthImag = 0;
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        if (asSingle) {
            formatInfo.widthReal = 14;
            formatInfo.widthImag = 14;
        } else {
            formatInfo.widthReal = 13;
            formatInfo.widthImag = 13;
        }
        formatInfo.formatReal = L"%*.*f";
        formatInfo.decimalsReal = 2;
        formatInfo.formatImag = L"%*.*f";
        formatInfo.decimalsImag = 2;
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        if (asSingle) {
            formatInfo.widthReal = 11;
            formatInfo.formatReal = L"%*s";
            formatInfo.decimalsReal = 0;
            formatInfo.widthImag = 11;
            formatInfo.formatImag = L"%*s";
            formatInfo.decimalsImag = 0;
        } else {
            formatInfo.widthReal = 19;
            formatInfo.formatReal = L"%*s";
            formatInfo.decimalsReal = 0;
            formatInfo.widthImag = 19;
            formatInfo.formatImag = L"%*s";
            formatInfo.decimalsImag = 0;
        }
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        formatInfo.widthReal = 9;
        formatInfo.decimalsReal = 0;
        formatInfo.formatReal = L"%*s";
        formatInfo.widthImag = 9;
        formatInfo.decimalsImag = 0;
        formatInfo.formatImag = L"%*s";
    } break;
    default: {
    } break;
    }
    return formatInfo;
}
//=============================================================================
static FormatDisplayInformation
computeFormatInfo(
    double realPart, double imagPart, bool asSingle, NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo
        = getArrayOfFormatInfo(asSingle, true, currentNumericFormat);

    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_SHORTE:
    case NLS_NUMERIC_FORMAT_SHORTG:
    case NLS_NUMERIC_FORMAT_LONGE:
    case NLS_NUMERIC_FORMAT_LONGG: {
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        formatInfo.formatReal = L"%*.*f";
        formatInfo.widthReal = 13;
        formatInfo.decimalsReal = 2;

        formatInfo.formatImag = L"%*.*f";
        formatInfo.widthImag = 13;
        formatInfo.decimalsImag = 2;
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        if (fabs(realPart) < 1e-10) {
            formatInfo.widthReal = 50;
            formatInfo.formatReal = L"%*s";
            formatInfo.trim = true;
        }
        if (fabs(realPart) > 1e10) {
            formatInfo.widthReal = 100;
            formatInfo.formatReal = L"%*s";
            formatInfo.trim = true;
        }

        if (fabs(imagPart) < 1e-10) {
            formatInfo.widthImag = 50;
            formatInfo.formatImag = L"%*s";
            formatInfo.trim = true;
        }
        if (fabs(imagPart) > 1e10) {
            formatInfo.widthImag = 100;
            formatInfo.formatImag = L"%*s";
            formatInfo.trim = true;
        }
    } break;
    default: {
        if (std::isfinite(realPart) || std::isfinite(imagPart)) {
            bool isSmallReal = (fabs(realPart) <= 1e-3);
            bool isSmallImag = (fabs(imagPart) <= 1e-3);
            bool isAllSmall = isSmallReal && isSmallImag;

            bool isBigReal = (fabs(realPart) >= 1e2);
            bool isBigImag = (fabs(imagPart) >= 1e2);
            bool isBig = isBigReal || isBigImag;
            bool engineer = false;
            if (realPart == 0 && imagPart == 0) {
                engineer = false;
            } else if (isBig || isAllSmall) {
                engineer = true;
            } else if (std::hypot(realPart, imagPart) == realPart
                || std::hypot(realPart, imagPart) == imagPart) {
                engineer = false;
            }
            if (engineer) {
                formatInfo.formatReal = L"%*.*e";
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 4;

                formatInfo.formatImag = L"%*.*e";
                formatInfo.widthImag = 11;
                formatInfo.decimalsImag = 4;

            } else {
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 9;
                formatInfo.decimalsReal = 4;

                formatInfo.formatImag = L"%*.*f";
                formatInfo.widthImag = 7;
                formatInfo.decimalsImag = 4;
            }
        } else {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 9;
            formatInfo.decimalsReal = 4;

            formatInfo.formatImag = L"%*.*f";
            formatInfo.widthImag = 7;
            formatInfo.decimalsImag = 4;
        }
    } break;
    }
    return formatInfo;
}
//=============================================================================
static FormatDisplayInformation
computeFormatInfo(double val, bool asSingle, NumericFormatDisplay currentNumericFormat)
{
    FormatDisplayInformation formatInfo
        = getArrayOfFormatInfo(asSingle, false, currentNumericFormat);

    switch (currentNumericFormat) {
    case NLS_NUMERIC_FORMAT_LONGG: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*g";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                if (asSingle) {
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;

                } else {
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
                formatInfo.formatReal = L"%*.*g";
            }
        }
    } break;

    case NLS_NUMERIC_FORMAT_SHORTG: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*g";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                formatInfo.widthReal = 13;
                formatInfo.formatReal = L"%*.*g";
                formatInfo.decimalsReal = 4;
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                if (asSingle) {
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
                formatInfo.formatReal = L"%*.*e";
                formatInfo.floatAsInteger = false;
            }
        }
    } break;

    case NLS_NUMERIC_FORMAT_SHORTE: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                if (asSingle) {
                    formatInfo.widthReal = 12;
                } else {
                    formatInfo.widthReal = 13;
                }
                formatInfo.formatReal = L"%*.*e";
                formatInfo.decimalsReal = 4;
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORT: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                formatInfo.formatReal = L"%*.*e";
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 4;
            }
        } else {
            if (absoluteValue <= 1e-3 || absoluteValue > 1e3) {
                formatInfo.formatReal = L"%*.*e";
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 4;
            } else {
                formatInfo.formatReal = L"%*.*f";
                formatInfo.widthReal = 10;
                formatInfo.decimalsReal = 4;
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            formatInfo.formatReal = L"%*.*f";
            formatInfo.widthReal = 6;
            formatInfo.decimalsReal = 4;
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 6;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                formatInfo.formatReal = L"%*ld";
                formatInfo.widthReal = 12;
                formatInfo.decimalsReal = 0;
                formatInfo.floatAsInteger = true;
            } else {
                formatInfo.formatReal = L"%*.*e";
                if (asSingle) {
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
            }
        } else {
            if (asSingle) {
                if (absoluteValue <= 1e-3 || absoluteValue > 1e2) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 7;
                }
            } else {
                if (absoluteValue <= 1e-3 || absoluteValue > 1e3) {
                    formatInfo.formatReal = L"%*.*e";
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                } else {
                    formatInfo.formatReal = L"%*.*f";
                    formatInfo.widthReal = 20;
                    formatInfo.decimalsReal = 15;
                }
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        if (fabs(val) < 1e-10) {
            formatInfo.widthReal = 50;
            formatInfo.formatReal = L"%*s";
            formatInfo.trim = true;
        }
        if (fabs(val) > 1e10) {
            formatInfo.widthReal = 100;
            formatInfo.formatReal = L"%*s";
            formatInfo.trim = true;
        }
    } break;
    default: {
    } break;
    }
    return formatInfo;
}
//=============================================================================
}
//=============================================================================

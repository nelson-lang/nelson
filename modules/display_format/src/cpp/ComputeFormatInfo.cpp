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
        formatInfo.formatReal = L"{:>{12}d"; // was: L"%*ld"
        formatInfo.floatAsInteger = true;
        int scaleFactor = (int)formatInfo.scaleFactor;
        switch (scaleFactor) {
        case 1:
        case 2: {
            formatInfo.widthReal = 6;
            formatInfo.formatReal = L"{:>6d}"; // was: L"%6ld"
        } break;
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
        case 8:
        default: {
            formatInfo.widthReal = 12;
            formatInfo.formatReal = L"{:>12d}"; // was: L"%12ld"
        } break;
        }
        formatInfo.decimalsReal = 0;
        return true;
    }
    return false;
}
//=============================================================================
static void
setRealFormat(FormatDisplayInformation& f, const std::wstring& fmt, size_t width, size_t decimals,
    bool floatAsInteger = false)
{
    f.formatReal = fmt;
    f.widthReal = width;
    f.decimalsReal = decimals;
    if (floatAsInteger) {
        f.floatAsInteger = true;
    }
}
//=============================================================================
static void
setImagFormat(FormatDisplayInformation& f, const std::wstring& fmt, size_t width, size_t decimals)
{
    f.formatImag = fmt;
    f.widthImag = width;
    f.decimalsImag = decimals;
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
                setRealFormat(formatInfo, L"{:>11.7f}", 11, 7);
                setImagFormat(formatInfo, L"{:>10.7f}", 10, 7);

            } else {
                setRealFormat(formatInfo, L"{:>19.15f}", 19, 15);
                setImagFormat(formatInfo, L"{:>18.15f}", 18, 15);
            }

        } break;
        case NLS_NUMERIC_FORMAT_SHORT: {
            ComputeScaleFactor(A, allInteger, formatInfo);
            formatInfo.isComplex = true;
            setRealFormat(formatInfo, L"{:>9.4f}", 9, 4);
            setImagFormat(formatInfo, L"{:>7.4f}", 7, 4);
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
                formatInfo.formatReal = L"{:>10.4f}"; // was: L"%10.4f"
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
                    formatInfo.formatReal = L"{:>12.7f}"; // was: L"%12.7f"
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"{:>20.15f}"; // was: L"%20.15f"
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
                    formatInfo.formatReal = L"{:>12.4e}"; // was: L"%12.4e"
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 4;
                } else {
                    formatInfo.formatReal = L"{:>13.4e}"; // was: L"%13.4e"
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
                    formatInfo.formatReal = L"{:>16.7e}"; // was: L"%16.7e"
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"{:>26.15e}"; // was: L"%26.15e"
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
                computeWidthRealByScaleFactor(formatInfo);
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_SHORTG: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.floatAsInteger = false;
                formatInfo.formatReal = L"{:>13.5g}"; // was: L"%13.5g"
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 5;
                computeWidthRealByScaleFactor(formatInfo);
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_LONGG: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                formatInfo.floatAsInteger = false;
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"{:>16.7g}"; // was: L"%16.7g"
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"{:>26.15g}"; // was: L"%26.15g"
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
                computeWidthRealByScaleFactor(formatInfo);
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_RATIONAL: {
                formatInfo.widthReal = 11;
                formatInfo.decimalsReal = 9;
                formatInfo.formatReal = L"{:>11}"; // was: L"%11s"
                formatInfo.scaleFactor = 1;
            } break;
            case NLS_NUMERIC_FORMAT_BANK: {
                formatInfo.widthReal = 14;
                formatInfo.widthImag = 14;
                formatInfo.formatReal = L"{:>14.2f}"; // was: L"%14.2f"
                formatInfo.decimalsReal = 2;
                formatInfo.formatImag = L"{:>14.2f}"; // was: L"%14.2f"
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
                    formatInfo.formatReal = L"{:>16.7g}"; // was: L"%16.7g"
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"{:>26.15g}"; // was: L"%26.15g"
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
            } break;
            case NLS_NUMERIC_FORMAT_SHORTG: {
                formatInfo.formatReal = L"{:>13.5g}"; // was: L"%13.5g"
                formatInfo.widthReal = 13;
                formatInfo.decimalsReal = 5;
            } break;
            case NLS_NUMERIC_FORMAT_SHORTE: {
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"{:>12.4e}"; // was: L"%12.4e"
                    formatInfo.widthReal = 12;
                    formatInfo.decimalsReal = 4;
                } else {
                    formatInfo.formatReal = L"{:>13.4e}"; // was: L"%13.4e"
                    formatInfo.widthReal = 13;
                    formatInfo.decimalsReal = 4;
                }
            } break;
            case NLS_NUMERIC_FORMAT_LONGE: {
                if (A.isSingleClass()) {
                    formatInfo.formatReal = L"{:>16.7e}"; // was: L"%16.7e"
                    formatInfo.widthReal = 16;
                    formatInfo.decimalsReal = 7;
                } else {
                    formatInfo.formatReal = L"{:>26.15e}"; // was: L"%26.15e"
                    formatInfo.widthReal = 26;
                    formatInfo.decimalsReal = 15;
                }
            } break;
            case NLS_NUMERIC_FORMAT_LONG: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                if (A.isSingleClass()) {
                    setRealFormat(formatInfo, L"{:>12.7f}", 12, 7);
                } else {
                    setRealFormat(formatInfo, L"{:>20.15f}", 20, 15);
                }
            } break;
            case NLS_NUMERIC_FORMAT_SHORT: {
                ComputeScaleFactor(A, allInteger, formatInfo);
                setRealFormat(formatInfo, L"{:>10.4f}", 10, 4);
            } break;
            case NLS_NUMERIC_FORMAT_RATIONAL: {
                setRealFormat(formatInfo, L"{:>11}", 11, 9);
            } break;
            case NLS_NUMERIC_FORMAT_BANK: {
                setRealFormat(formatInfo, L"{:>14.2f}", 14, 2);
                setImagFormat(formatInfo, L"{:>14.2f}", 14, 2);
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
        setRealFormat(formatInfo, L"{:>9.4f}", 9, 4);
        setImagFormat(formatInfo, L"{:>9.4f}", 9, 4);
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        if (asSingle) {
            setRealFormat(formatInfo, L"{:>18.7f}", 18, 7, false);
            setImagFormat(formatInfo, L"{:>18.7f}", 18, 7);
        } else {
            setRealFormat(formatInfo, L"{:>18.15f}", 18, 18, false);
            setImagFormat(formatInfo, L"{:>18.15f}", 18, 18);
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORTE: {
        if (asSingle) {
            setRealFormat(formatInfo, L"{:>12.4e}", 12, 4);
            setImagFormat(formatInfo, L"{:>10.4e}", 10, 4);
        } else {
            setRealFormat(formatInfo, L"{:>13.4e}", 13, 4);
            setImagFormat(formatInfo, L"{:>11.4e}", 11, 4);
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
        if (asSingle) {
            if (asComplex) {
                setRealFormat(formatInfo, L"{:>15.7e}", 15, 7);
                setImagFormat(formatInfo, L"{:>14.7e}", 14, 7);
            } else {
                setRealFormat(formatInfo, L"{:>16.7e}", 16, 7);
            }
        } else {
            formatInfo.formatReal = L"{:>26.15e}"; // was: L"%*.*e"
            formatInfo.decimalsReal = 15;
            if (asComplex) {
                formatInfo.widthReal = 27;
                formatInfo.formatReal = L"{:>27.15e}"; // was: L"%*.*e"
                setRealFormat(formatInfo, L"{:>27.15e}", 27, 15);
                setImagFormat(formatInfo, L"{:>22.15e}", 22, 15);
            } else {
                setRealFormat(formatInfo, L"{:>26.15e}", 26, 15);
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORTG: {
        if (asSingle) {
            if (asComplex) {
                setRealFormat(formatInfo, L"{:>12.5g}", 12, 5);
                setImagFormat(formatInfo, L"{:>11.5g}", 11, 5);
            } else {
                setRealFormat(formatInfo, L"{:>13.5g}", 13, 5);
            }
        } else {
            setRealFormat(formatInfo, L"{:>13.5g}", 13, 5);
            if (asComplex) {
                setImagFormat(formatInfo, L"{:>11.5g}", 11, 5);
            } else {
                setImagFormat(formatInfo, L"{:>0.0g}", 0, 0);
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGG: {
        if (asSingle) {
            if (asComplex) {
                setRealFormat(formatInfo, L"{:>14.7g}", 14, 7);
                setImagFormat(formatInfo, L"{:>13.7g}", 13, 7);
            } else {
                setRealFormat(formatInfo, L"{:>16.7g}", 16, 7);
            }
        } else {
            if (asComplex) {
                setRealFormat(formatInfo, L"{:>27.15g}", 27, 15);
                setImagFormat(formatInfo, L"{:>22.15g}", 22, 15);
            } else {
                setRealFormat(formatInfo, L"{:>26.15g}", 26, 15);
                setImagFormat(formatInfo, L"{:>0.0g}", 0, 0);
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORTENG: {
        if (asSingle) {
            if (asComplex) {
                setRealFormat(formatInfo, L"{:>13.7e}", 13, 7);
                setImagFormat(formatInfo, L"{:>13.7e}", 13, 7);

            } else {
                formatInfo.widthReal = 17;
                formatInfo.decimalsReal = 16;
                setRealFormat(formatInfo, L"{:>17.16e}", 17, 16);
            }
        } else {
            if (asComplex) {
                setRealFormat(formatInfo, L"{:>17.0e}", 17, 0);
                setImagFormat(formatInfo, L"{:>13.0e}", 13, 0);
            } else {
                setRealFormat(formatInfo, L"{:>17.17e}", 17, 17);
                setImagFormat(formatInfo, L"{:>0.0e}", 0, 0);
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGENG: {
        if (asSingle) {
            if (asComplex) {
                setRealFormat(formatInfo, L"{:>34.0e}", 34, 0);
                setImagFormat(formatInfo, L"{:>34.0e}", 34, 0);
            } else {
                setRealFormat(formatInfo, L"{:>17.17e}", 17, 17);
            }
        } else {
            if (asComplex) {
                setRealFormat(formatInfo, L"{:>51.0e}", 51, 0);
                setImagFormat(formatInfo, L"{:>51.0e}", 51, 0);
            } else {
                setRealFormat(formatInfo, L"{:>26.26e}", 26, 26);
                setImagFormat(formatInfo, L"{:>0.0e}", 0, 0);
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_PLUS: {
        formatInfo.widthReal = 1;
        formatInfo.widthImag = 0;
    } break;
    case NLS_NUMERIC_FORMAT_BANK: {
        if (asSingle) {
            setRealFormat(formatInfo, L"{:>14.2f}", 14, 2);
            setImagFormat(formatInfo, L"{:>14.2f}", 14, 2);

        } else {
            setRealFormat(formatInfo, L"{:>13.2f}", 13, 2);
            setImagFormat(formatInfo, L"{:>13.2f}", 13, 2);
        }
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
        if (asSingle) {
            setRealFormat(formatInfo, L"{:>11}", 11, 0);
            setImagFormat(formatInfo, L"{:>11}", 11, 0);
        } else {
            setRealFormat(formatInfo, L"{:>19}", 19, 0);
            setImagFormat(formatInfo, L"{:>19}", 19, 0);
        }
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        setRealFormat(formatInfo, L"{:>9}", 9, 0);
        setImagFormat(formatInfo, L"{:>9}", 9, 0);
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
        setRealFormat(formatInfo, L"{:>13.2f}", 13, 2);
        setImagFormat(formatInfo, L"{:>13.2f}", 13, 2);
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        if (fabs(realPart) < 1e-10) {
            setRealFormat(formatInfo, L"{:>50}", 50, formatInfo.decimalsReal); // was: L"%50s"
            formatInfo.trim = true;
        }
        if (fabs(realPart) > 1e10) {
            setRealFormat(formatInfo, L"{:>100}", 100, formatInfo.decimalsReal); // was: L"%100s"
            formatInfo.trim = true;
        }

        if (fabs(imagPart) < 1e-10) {
            setImagFormat(formatInfo, L"{:>50}", 50, formatInfo.decimalsImag); // was: L"%50s"
            formatInfo.trim = true;
        }
        if (fabs(imagPart) > 1e10) {
            setImagFormat(formatInfo, L"{:>100}", 100, formatInfo.decimalsImag); // was: L"%100s"
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
                setRealFormat(formatInfo, L"{:>13.4e}", 13, 4);
                setImagFormat(formatInfo, L"{:>11.4e}", 11, 4);
            } else {
                setRealFormat(formatInfo, L"{:>9.4f}", 9, 4);
                setImagFormat(formatInfo, L"{:>7.4f}", 7, 4);
            }
        } else {
            setRealFormat(formatInfo, L"{:>9.4f}", 9, 4);
            setImagFormat(formatInfo, L"{:>7.4f}", 7, 4);
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
            setRealFormat(formatInfo, L"{:>6.4g}", 6, 4);
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                setRealFormat(formatInfo, L"{:>6d}", 6, 0, true);
            } else if (absoluteValue <= 999999999) {
                setRealFormat(formatInfo, L"{:>12d}", 12, 0, true);
            } else {
                if (asSingle) {
                    setRealFormat(formatInfo, L"{:>16.7g}", 16, 7);
                } else {
                    setRealFormat(formatInfo, L"{:>26.15g}", 26, 15);
                }
            }
        }
    } break;

    case NLS_NUMERIC_FORMAT_SHORTG: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            setRealFormat(formatInfo, L"{:>6.4g}", 6, 4);
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                setRealFormat(formatInfo, L"{:6d}", 6, 0, true);
            } else if (absoluteValue <= 999999999) {
                setRealFormat(formatInfo, L"{:>12d}", 12, 0, true);
            } else {
                setRealFormat(formatInfo, L"{:>13.4g}", 13, 4);
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONGE: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            setRealFormat(formatInfo, L"{:>6.4f}", 6, 4);
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                setRealFormat(formatInfo, L"{:>6d}", 6, 0, true);
            } else if (absoluteValue <= 999999999) {
                setRealFormat(formatInfo, L"{:>12d}", 12, 0, true);
            } else {
                if (asSingle) {
                    setRealFormat(formatInfo, L"{:>16.7e}", 16, 7);

                } else {
                    setRealFormat(formatInfo, L"{:>26.15e}", 26, 15);
                }
                formatInfo.floatAsInteger = false;
            }
        }
    } break;

    case NLS_NUMERIC_FORMAT_SHORTE: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            setRealFormat(formatInfo, L"{:>6.4f}", 6, 4);
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                setRealFormat(formatInfo, L"{:>6d}", 6, 0, true);
            } else if (absoluteValue <= 999999999) {
                setRealFormat(formatInfo, L"{:>12d}", 12, 0, true);
            } else {
                if (asSingle) {
                    setRealFormat(formatInfo, L"{:>12.4e}", 12, 4);
                } else {
                    setRealFormat(formatInfo, L"{:>13.4e}", 13, 4);
                }
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_SHORT: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            setRealFormat(formatInfo, L"{:>6.4f}", 6, 4);

        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                setRealFormat(formatInfo, L"{:>6d}", 6, 0, true);
            } else if (absoluteValue <= 999999999) {
                setRealFormat(formatInfo, L"{:>12d}", 12, 0, true);
            } else {
                setRealFormat(formatInfo, L"{:>13.4e}", 13, 4);
            }
        } else {
            if (absoluteValue <= 1e-3 || absoluteValue > 1e3) {
                setRealFormat(formatInfo, L"{:>13.4e}", 13, 4);
            } else {
                setRealFormat(formatInfo, L"{:>10.4f}", 10, 4);
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_LONG: {
        double absoluteValue = fabs(val);
        if (!std::isfinite(absoluteValue)) {
            setRealFormat(formatInfo, L"{:>6.4f}", 6, 4);
        } else if (IsIntegerForm(val)) {
            if (absoluteValue <= 999) {
                setRealFormat(formatInfo, L"{:>6d}", 6, 0, true);
                formatInfo.floatAsInteger = true;
            } else if (absoluteValue <= 999999999) {
                setRealFormat(formatInfo, L"{:>12d}", 12, 0, true);
            } else {
                if (asSingle) {
                    setRealFormat(formatInfo, L"{:>16.7e}", 16, 7);

                } else {
                    setRealFormat(formatInfo, L"{:>26.15e}", 26, 15);
                }
            }
        } else {
            if (asSingle) {
                if (absoluteValue <= 1e-3 || absoluteValue > 1e2) {
                    setRealFormat(formatInfo, L"{:>16.7e}", 16, 7);
                } else {
                    setRealFormat(formatInfo, L"{:>12.7f}", 12, 7);
                    formatInfo.decimalsReal = 7;
                }
            } else {
                if (absoluteValue <= 1e-3 || absoluteValue > 1e3) {
                    setRealFormat(formatInfo, L"{:>26.15e}", 26, 15);
                } else {
                    setRealFormat(formatInfo, L"{:>20.15f}", 20, 15);
                }
            }
        }
    } break;
    case NLS_NUMERIC_FORMAT_HEX: {
    } break;
    case NLS_NUMERIC_FORMAT_RATIONAL: {
        if (fabs(val) < 1e-10) {
            setRealFormat(formatInfo, L"{:>50}", 50, 0);
            formatInfo.trim = true;
        }
        if (fabs(val) > 1e10) {
            setRealFormat(formatInfo, L"{:>100}", 100, 0);
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

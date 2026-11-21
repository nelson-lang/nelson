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
#include "SimEngineTypes.hpp"
#include <complex>
#include <vector>
#include <string>
#include <algorithm>
//=============================================================================
namespace Nelson::NFlow {
inline bool
isMatrixProduct(const Block& b)
{
    const auto& p = asJson(b.params);
    const std::string mode = p.value("Multiplication", std::string());
    return mode == "Matrix(*)" || mode == "matrix";
}
//=============================================================================
inline bool
resolveMatrixProduct(const Block& b, const std::vector<PortSig>& inputs,
    const std::vector<int>& connected, PortSig& output, std::string& error)
{
    const auto& p = asJson(b.params);
    const std::string ops = p.contains("Inputs") && p["Inputs"].is_string()
        ? p["Inputs"].get<std::string>()
        : std::string();
    int rows = 1, cols = 1;
    bool haveType = false;
    for (size_t i = 0; i < inputs.size(); ++i) {
        if (i < connected.size() && connected[i] < 0) {
            continue;
        }
        const auto& in = inputs[i];
        if (!haveType) {
            output.type = in.type;
            haveType = true;
        }
        if (in.width() > 1 && in.numDims() > 2) {
            error = "matrix Product requires 2-D inputs";
            return false;
        }
        const bool divide = i < ops.size() && ops[i] == '/';
        if (divide && in.width() > 1
            && ((in.type != SigType::Double && in.type != SigType::Single)
                || (output.type != SigType::Double && output.type != SigType::Single))) {
            error = "integer matrix division is not supported; convert inputs to single or double";
            return false;
        }
        if (divide && in.width() > 1 && in.rows() != in.cols()) {
            error = "matrix Product divisor must be square";
            return false;
        }
        if (in.width() <= 1) {
            continue;
        }
        if (rows == 1 && cols == 1) {
            rows = in.rows();
            cols = in.cols();
            continue;
        }
        if (cols != in.rows()) {
            error = "matrix Product dimension mismatch";
            return false;
        }
        cols = in.cols();
    }
    if (rows == 1 && cols == 1) {
        output.setScalar();
    } else {
        output.setShape2D(rows, cols);
    }
    return true;
}
//=============================================================================
struct ProductMatrix
{
    int rows = 1, cols = 1;
    std::vector<std::complex<double>> values { { 1.0, 0.0 } };
};
//=============================================================================
inline std::complex<double>
quantizeProductValue(std::complex<double> value, SigType type, bool saturate)
{
    if (type == SigType::Double) {
        return value;
    }
    return { quantizeToType(value.real(), type, saturate),
        quantizeToType(value.imag(), type, saturate) };
}
//=============================================================================
inline bool
invertProductMatrix(ProductMatrix& m, SigType type, bool saturate)
{
    if (m.rows != m.cols) {
        return false;
    }
    const int n = m.rows;
    std::vector<std::complex<double>> inverse(n * n);
    for (int i = 0; i < n; ++i) {
        inverse[i * n + i] = 1.0;
    }
    for (int c = 0; c < n; ++c) {
        int pivot = c;
        for (int r = c + 1; r < n; ++r) {
            if (std::abs(m.values[c * n + r]) > std::abs(m.values[c * n + pivot])) {
                pivot = r;
            }
        }
        if (std::abs(m.values[c * n + pivot]) == 0.0) {
            return false;
        }
        for (int j = 0; j < n; ++j) {
            std::swap(m.values[j * n + c], m.values[j * n + pivot]);
            std::swap(inverse[j * n + c], inverse[j * n + pivot]);
        }
        const auto divisor = m.values[c * n + c];
        for (int j = 0; j < n; ++j) {
            m.values[j * n + c]
                = quantizeProductValue(m.values[j * n + c] / divisor, type, saturate);
            inverse[j * n + c] = quantizeProductValue(inverse[j * n + c] / divisor, type, saturate);
        }
        for (int r = 0; r < n; ++r) {
            if (r == c) {
                continue;
            }
            const auto factor = m.values[c * n + r];
            for (int j = 0; j < n; ++j) {
                const auto term
                    = quantizeProductValue(factor * m.values[j * n + c], type, saturate);
                const auto invTerm
                    = quantizeProductValue(factor * inverse[j * n + c], type, saturate);
                m.values[j * n + r]
                    = quantizeProductValue(m.values[j * n + r] - term, type, saturate);
                inverse[j * n + r]
                    = quantizeProductValue(inverse[j * n + r] - invTerm, type, saturate);
            }
        }
    }
    m.values = std::move(inverse);
    return true;
}
//=============================================================================
inline ProductMatrix
multiplyProductMatrices(const ProductMatrix& a, const ProductMatrix& b, SigType type, bool saturate)
{
    if (a.values.size() == 1 || b.values.size() == 1) {
        ProductMatrix out = a.values.size() == 1 ? b : a;
        const auto scalar = a.values.size() == 1 ? a.values[0] : b.values[0];
        for (auto& value : out.values) {
            value = quantizeProductValue(value * scalar, type, saturate);
        }
        return out;
    }
    ProductMatrix out;
    out.rows = a.rows;
    out.cols = b.cols;
    out.values.assign(out.rows * out.cols, {});
    for (int c = 0; c < b.cols; ++c) {
        for (int r = 0; r < a.rows; ++r) {
            for (int k = 0; k < a.cols; ++k) {
                const auto term = quantizeProductValue(
                    a.values[k * a.rows + r] * b.values[c * b.rows + k], type, saturate);
                out.values[c * out.rows + r]
                    = quantizeProductValue(out.values[c * out.rows + r] + term, type, saturate);
            }
        }
    }
    return out;
}
//=============================================================================
} // namespace Nelson::NFlow
//=============================================================================

//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "BWMorph.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <cwctype>
#include <limits>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
enum class BWMorphOperation
{
    Clean,
    Fill,
    Majority,
    Remove,
    Endpoints,
    Branchpoints,
    Spur,
    Bridge,
    Thin,
    Skel,
    Dilate,
    Erode,
    Open,
    Close,
    Tophat,
    Bothat
};
//=============================================================================
static std::wstring
lowerText(std::wstring text)
{
    std::transform(text.begin(), text.end(), text.begin(), ::towlower);
    return text;
}
//=============================================================================
static BWMorphOperation
parseOperation(const std::wstring& operation)
{
    std::wstring op = lowerText(operation);
    if (op == L"clean") {
        return BWMorphOperation::Clean;
    }
    if (op == L"fill") {
        return BWMorphOperation::Fill;
    }
    if (op == L"majority") {
        return BWMorphOperation::Majority;
    }
    if (op == L"remove") {
        return BWMorphOperation::Remove;
    }
    if (op == L"endpoints") {
        return BWMorphOperation::Endpoints;
    }
    if (op == L"branchpoints") {
        return BWMorphOperation::Branchpoints;
    }
    if (op == L"spur") {
        return BWMorphOperation::Spur;
    }
    if (op == L"bridge") {
        return BWMorphOperation::Bridge;
    }
    if (op == L"thin") {
        return BWMorphOperation::Thin;
    }
    if (op == L"skel") {
        return BWMorphOperation::Skel;
    }
    if (op == L"dilate") {
        return BWMorphOperation::Dilate;
    }
    if (op == L"erode") {
        return BWMorphOperation::Erode;
    }
    if (op == L"open") {
        return BWMorphOperation::Open;
    }
    if (op == L"close") {
        return BWMorphOperation::Close;
    }
    if (op == L"tophat") {
        return BWMorphOperation::Tophat;
    }
    if (op == L"bothat") {
        return BWMorphOperation::Bothat;
    }
    Error(L"Nelson:image_processing:unsupportedBwmorphOperation", L"Unsupported \"%s\" option.",
        L"bwmorph operation");
    return BWMorphOperation::Clean;
}
//=============================================================================
static void
validateLogicalImage2D(const ArrayOf& image)
{
    if (!image.isLogical() || image.isSparse() || image.dimensions().length() > 2) {
        Error(L"Nelson:bwmorph:invalidImage", L"Image must be a 2-D logical array.");
    }
}
//=============================================================================
static int
neighborCount(
    const std::vector<uint8_t>& input, indexType rows, indexType cols, indexType row, indexType col)
{
    int count = 0;
    for (int dc = -1; dc <= 1; dc++) {
        for (int dr = -1; dr <= 1; dr++) {
            if (dr == 0 && dc == 0) {
                continue;
            }
            signedIndexType rr = static_cast<signedIndexType>(row) + dr;
            signedIndexType cc = static_cast<signedIndexType>(col) + dc;
            if (rr >= 0 && cc >= 0 && rr < static_cast<signedIndexType>(rows)
                && cc < static_cast<signedIndexType>(cols)
                && input[static_cast<indexType>(rr) + static_cast<indexType>(cc) * rows]) {
                count++;
            }
        }
    }
    return count;
}
//=============================================================================
static bool
bridgeCandidate(
    const std::vector<uint8_t>& input, indexType rows, indexType cols, indexType row, indexType col)
{
    const std::array<int, 8> dRows { -1, 0, 1, -1, 1, -1, 0, 1 };
    const std::array<int, 8> dCols { -1, -1, -1, 0, 0, 1, 1, 1 };
    std::array<uint8_t, 8> present {};
    int count = 0;
    for (size_t k = 0; k < present.size(); k++) {
        signedIndexType rr = static_cast<signedIndexType>(row) + dRows[k];
        signedIndexType cc = static_cast<signedIndexType>(col) + dCols[k];
        present[k] = rr >= 0 && cc >= 0 && rr < static_cast<signedIndexType>(rows)
            && cc < static_cast<signedIndexType>(cols)
            && input[static_cast<indexType>(rr) + static_cast<indexType>(cc) * rows];
        count += present[k] ? 1 : 0;
    }
    if (count < 2) {
        return false;
    }
    std::array<uint8_t, 8> visited {};
    int components = 0;
    for (size_t start = 0; start < present.size(); start++) {
        if (!present[start] || visited[start]) {
            continue;
        }
        components++;
        if (components >= 2) {
            return true;
        }
        std::array<size_t, 8> pending {};
        size_t pendingSize = 1;
        pending[0] = start;
        visited[start] = 1;
        for (size_t cursor = 0; cursor < pendingSize; cursor++) {
            size_t current = pending[cursor];
            for (size_t next = 0; next < present.size(); next++) {
                if (present[next] && !visited[next] && std::abs(dRows[current] - dRows[next]) <= 1
                    && std::abs(dCols[current] - dCols[next]) <= 1) {
                    visited[next] = 1;
                    pending[pendingSize++] = next;
                }
            }
        }
    }
    return false;
}
//=============================================================================
static void
applyOnce(const std::vector<uint8_t>& input, std::vector<uint8_t>& output, indexType rows,
    indexType cols, BWMorphOperation operation)
{
    indexType total = rows * cols;
    OMP_PARALLEL_FOR_LOOP(total, 4)
    for (signedIndexType kSigned = 0; kSigned < static_cast<signedIndexType>(total); kSigned++) {
        indexType k = static_cast<indexType>(kSigned);
        indexType row = k % rows;
        indexType col = k / rows;
        uint8_t value = input[k] ? 1 : 0;
        int neighbors = operation == BWMorphOperation::Bridge
            ? 0
            : neighborCount(input, rows, cols, row, col);
        switch (operation) {
        case BWMorphOperation::Clean:
            output[k] = value && neighbors > 0;
            break;
        case BWMorphOperation::Fill:
            output[k] = value || (!value && neighbors == 8);
            break;
        case BWMorphOperation::Majority:
            output[k] = (neighbors + value) >= 5;
            break;
        case BWMorphOperation::Remove:
            output[k] = value && neighbors < 8;
            break;
        case BWMorphOperation::Endpoints:
            output[k] = value && neighbors == 1;
            break;
        case BWMorphOperation::Branchpoints:
            output[k] = value && neighbors >= 3;
            break;
        case BWMorphOperation::Spur:
            output[k] = value && neighbors != 1;
            break;
        case BWMorphOperation::Bridge:
            output[k] = value || bridgeCandidate(input, rows, cols, row, col);
            break;
        case BWMorphOperation::Thin:
        case BWMorphOperation::Skel:
        case BWMorphOperation::Open:
        case BWMorphOperation::Close:
        case BWMorphOperation::Tophat:
        case BWMorphOperation::Bothat:
            output[k] = value;
            break;
        case BWMorphOperation::Dilate:
            output[k] = value || neighbors > 0;
            break;
        case BWMorphOperation::Erode:
            output[k] = value && neighbors == 8;
            break;
        }
    }
}
//=============================================================================
static void
applyBinaryOnce(const std::vector<uint8_t>& input, std::vector<uint8_t>& output, indexType rows,
    indexType cols, BWMorphOperation operation)
{
    applyOnce(input, output, rows, cols, operation);
}
//=============================================================================
static bool
applyCompositeOperation(std::vector<uint8_t>& current, std::vector<uint8_t>& next,
    std::vector<uint8_t>& work, indexType rows, indexType cols, BWMorphOperation operation)
{
    std::vector<uint8_t> original;
    bool differenceAgainstOriginal
        = operation == BWMorphOperation::Tophat || operation == BWMorphOperation::Bothat;
    if (differenceAgainstOriginal) {
        original = current;
    }
    if (operation == BWMorphOperation::Open || operation == BWMorphOperation::Tophat) {
        applyBinaryOnce(current, work, rows, cols, BWMorphOperation::Erode);
        applyBinaryOnce(work, next, rows, cols, BWMorphOperation::Dilate);
    } else {
        applyBinaryOnce(current, work, rows, cols, BWMorphOperation::Dilate);
        applyBinaryOnce(work, next, rows, cols, BWMorphOperation::Erode);
    }
    if (operation == BWMorphOperation::Tophat) {
        for (size_t k = 0; k < next.size(); k++) {
            next[k] = original[k] && !next[k];
        }
    } else if (operation == BWMorphOperation::Bothat) {
        for (size_t k = 0; k < next.size(); k++) {
            next[k] = next[k] && !original[k];
        }
    }
    bool changed = next != current;
    current.swap(next);
    return changed;
}
//=============================================================================
static uint8_t
pixelAt(const std::vector<uint8_t>& input, indexType rows, indexType cols, signedIndexType row,
    signedIndexType col)
{
    if (row < 0 || col < 0 || row >= static_cast<signedIndexType>(rows)
        || col >= static_cast<signedIndexType>(cols)) {
        return 0;
    }
    return input[static_cast<indexType>(row) + static_cast<indexType>(col) * rows] ? 1 : 0;
}
//=============================================================================
static bool
thinDeletionCandidate(const std::vector<uint8_t>& input, indexType rows, indexType cols,
    indexType row, indexType col, int pass)
{
    indexType idx = row + col * rows;
    if (!input[idx]) {
        return false;
    }
    signedIndexType r = static_cast<signedIndexType>(row);
    signedIndexType c = static_cast<signedIndexType>(col);
    std::array<uint8_t, 8> p { pixelAt(input, rows, cols, r - 1, c),
        pixelAt(input, rows, cols, r - 1, c + 1), pixelAt(input, rows, cols, r, c + 1),
        pixelAt(input, rows, cols, r + 1, c + 1), pixelAt(input, rows, cols, r + 1, c),
        pixelAt(input, rows, cols, r + 1, c - 1), pixelAt(input, rows, cols, r, c - 1),
        pixelAt(input, rows, cols, r - 1, c - 1) };
    int neighbors = 0;
    int transitions = 0;
    for (size_t k = 0; k < p.size(); k++) {
        neighbors += p[k] ? 1 : 0;
        transitions += (!p[k] && p[(k + 1) % p.size()]) ? 1 : 0;
    }
    if (neighbors < 2 || neighbors > 6 || transitions != 1) {
        return false;
    }
    if (pass == 0) {
        return !(p[0] && p[2] && p[4]) && !(p[2] && p[4] && p[6]);
    }
    return !(p[0] && p[2] && p[6]) && !(p[0] && p[4] && p[6]);
}
//=============================================================================
static bool
thinSubiteration(std::vector<uint8_t>& current, std::vector<uint8_t>& next, indexType rows,
    indexType cols, int pass)
{
    indexType total = rows * cols;
    std::copy(current.begin(), current.end(), next.begin());
    OMP_PARALLEL_FOR_LOOP(total, 4)
    for (signedIndexType kSigned = 0; kSigned < static_cast<signedIndexType>(total); kSigned++) {
        indexType k = static_cast<indexType>(kSigned);
        indexType row = k % rows;
        indexType col = k / rows;
        if (thinDeletionCandidate(current, rows, cols, row, col, pass)) {
            next[k] = 0;
        }
    }
    bool changed = next != current;
    current.swap(next);
    return changed;
}
//=============================================================================
static bool
thinIteration(
    std::vector<uint8_t>& current, std::vector<uint8_t>& next, indexType rows, indexType cols)
{
    bool changedFirst = thinSubiteration(current, next, rows, cols, 0);
    bool changedSecond = thinSubiteration(current, next, rows, cols, 1);
    return changedFirst || changedSecond;
}
//=============================================================================
static bool
singlePassOperation(BWMorphOperation operation)
{
    return operation == BWMorphOperation::Endpoints || operation == BWMorphOperation::Branchpoints;
}
//=============================================================================
static bool
thinningOperation(BWMorphOperation operation)
{
    return operation == BWMorphOperation::Thin || operation == BWMorphOperation::Skel;
}
//=============================================================================
static bool
compositeOperation(BWMorphOperation operation)
{
    return operation == BWMorphOperation::Open || operation == BWMorphOperation::Close
        || operation == BWMorphOperation::Tophat || operation == BWMorphOperation::Bothat;
}
//=============================================================================
ArrayOf
BWMorph(const ArrayOf& image, const std::wstring& operationName, double iterations)
{
    validateLogicalImage2D(image);
    BWMorphOperation operation = parseOperation(operationName);
    indexType rows = image.rows();
    indexType cols = image.columns();
    indexType total = rows * cols;
    Dimensions dims = image.dimensions();
    ArrayOf out(NLS_LOGICAL, dims, ArrayOf::allocateArrayOf(NLS_LOGICAL, total));
    const auto* inputPtr = static_cast<const logical*>(image.dataPointer());
    std::vector<uint8_t> current(total);
    for (indexType k = 0; k < total; k++) {
        current[k] = inputPtr[k] ? 1 : 0;
    }
    if (iterations == 0 || image.isEmpty()) {
        std::copy(
            current.begin(), current.end(), static_cast<logical*>(out.readWriteDataPointer()));
        return out;
    }
    std::vector<uint8_t> next(total);
    std::vector<uint8_t> work(total);
    indexType maxIterations = std::isinf(iterations) ? std::numeric_limits<indexType>::max()
                                                     : static_cast<indexType>(iterations);
    if (singlePassOperation(operation)) {
        maxIterations = 1;
    }
    for (indexType iter = 0; iter < maxIterations; iter++) {
        if (thinningOperation(operation)) {
            bool changed = thinIteration(current, next, rows, cols);
            if (!changed) {
                break;
            }
            continue;
        }
        if (compositeOperation(operation)) {
            if (!applyCompositeOperation(current, next, work, rows, cols, operation)) {
                break;
            }
            continue;
        }
        applyOnce(current, next, rows, cols, operation);
        if (next == current) {
            break;
        }
        current.swap(next);
    }
    std::copy(current.begin(), current.end(), static_cast<logical*>(out.readWriteDataPointer()));
    return out;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <rapidcsv.h>
#include "ReadCell.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static void
ConvertToArrayOf(const std::string& pStr, ArrayOf& pVal)
{
    pVal = ArrayOf::characterArrayConstructor(pStr);
}
//=============================================================================
static char
detectSeparator(const std::string& filename)
{
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error opening file!" << std::endl;
        return '\0';
    }

    std::string line;
    if (std::getline(file, line)) {
        // Count occurrences of potential separators
        std::unordered_map<char, int> separatorCount;
        for (char sep : { ',', ';', '\t' }) {
            separatorCount[sep] = 0;
        }

        // Increment counts for each potential separator
        for (char ch : line) {
            if (separatorCount.find(ch) != separatorCount.end()) {
                separatorCount[ch]++;
            }
        }

        // Find the separator with the most occurrences
        char likelySeparator = '\0';
        int maxCount = 0;
        for (const auto& entry : separatorCount) {
            if (entry.second > maxCount) {
                likelySeparator = entry.first;
                maxCount = entry.second;
            }
        }

        return likelySeparator;
    }

    return '\0'; // Return null character if no valid separator found
}
//=============================================================================
ArrayOf
ReadCell(const std::wstring& filename, std::wstring& errorMessage)
{
    try {
        char separator = detectSeparator(wstring_to_utf8(filename));

        rapidcsv::Document doc(wstring_to_utf8(filename), rapidcsv::LabelParams(),
            rapidcsv::SeparatorParams(separator));
        stringVector columnNames = doc.GetColumnNames();
        size_t nbRows = doc.GetRowCount();
        size_t nbColumns = doc.GetColumnCount();
        size_t nbElements = nbRows * nbColumns;

        ArrayOf* elements = (ArrayOf*)ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nbElements);
        Dimensions dims(nbRows, nbColumns);
        ArrayOf result = ArrayOf(NLS_CELL_ARRAY, dims, elements);
        /*
        for (size_t i = 0; i < nbColumns; ++i) {
            for (size_t j = 0; j < nbRows; ++j) {

                size_t index = i * nbRows + j; // Corrected index calculation
                elements[index] = doc.GetCell<ArrayOf>(i, j, ConvertToArrayOf);
            }
        }
        */
        for (size_t index = 0; index < nbColumns * nbRows; ++index) {
            size_t i = index / nbRows; // Calculate the column index
            size_t j = index % nbRows; // Calculate the row index

            elements[index] = doc.GetCell<ArrayOf>(i, j, ConvertToArrayOf);
        }
        return result;
    } catch (const std::exception& e) {
        errorMessage = utf8_to_wstring(e.what());
        return ArrayOf(); // Return an empty ArrayOf on error
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================

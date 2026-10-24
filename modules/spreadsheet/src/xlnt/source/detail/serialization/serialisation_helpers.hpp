#ifndef XLNT_DETAIL_SERIALISATION_HELPERS_HPP
#define XLNT_DETAIL_SERIALISATION_HELPERS_HPP

#include <xlnt/cell/cell_type.hpp>
#include <xlnt/cell/index_types.hpp>
#include <detail/xlnt_config_impl.hpp>

#include <string>

namespace xlnt {
namespace detail {

    /// parsing assumptions used by the following functions
    /// - on entry, the start element for the element has been consumed by parser->next
    /// - on exit, the closing element has been consumed by parser->next
    /// using these assumptions, the following functions DO NOT use parser->peek (SLOW!!!)
    /// probable further gains from not building an attribute map and using the attribute events
    /// instead as the impl just iterates the map

    /// 'r' == cell reference e.g. 'A1'
    /// https://docs.microsoft.com/en-us/openspecs/office_standards/ms-oe376/db11a912-b1cb-4dff-b46d-9bedfd10cef0
    ///
    /// a lightweight version of xlnt::cell_reference with no extre functionality
    /// (absolute/relative, ...) many thousands are created during (de)serialisation, so even minor
    /// overhead is noticable
    struct Cell_Reference
    {
        // the obvious ctor
        explicit Cell_Reference(xlnt::row_t row_arg, xlnt::column_t::index_t column_arg) noexcept
            : row(row_arg), column(column_arg)
        {
        }

        // the common case. row # is already known during parsing (from parent <row> element)
        // just need to evaluate the column
        explicit Cell_Reference(xlnt::row_t row_arg, const std::string& reference) noexcept
            : row(row_arg)
        {
            // only three characters allowed for the column
            // assumption:
            // - regex pattern match: [A-Z]{1,3}\d{1,7}
            const char* iter = reference.c_str();
            int temp = *iter - 'A' + 1; // 'A' == 1
            ++iter;
            if (*iter >= 'A') // second char
            {
                temp *= 26; // LHS values are more significant
                temp += *iter - 'A' + 1; // 'A' == 1
                ++iter;
                if (*iter >= 'A') // third char
                {
                    temp *= 26; // LHS values are more significant
                    temp += *iter - 'A' + 1; // 'A' == 1
                }
            }
            column = static_cast<xlnt::column_t::index_t>(temp);
        }

        // for sorting purposes
        bool
        operator<(const Cell_Reference& rhs)
        {
            // row first, serialisation is done by row then column
            if (row < rhs.row) {
                return true;
            } else if (rhs.row < row) {
                return false;
            }
            // same row, column comparison
            return column < rhs.column;
        }

        // According to the OOXML specification:
        // "In SpreadsheetML, cell references range from column A1–A1048576 (column A:A) to column
        // XFD1–XFD1048576 (column XFD:XFD). An implementation can extend this range."
        xlnt::row_t row; // default range: [1, 1048576], but XLNT allows [1, 4294967295]
        xlnt::column_t::index_t
            column; // default range: ["A", "XFD"] -> [1, 16384], but XLNT allows [1, 4294967295]
    };

    // <c> inside <row> element
    // https://docs.microsoft.com/en-us/dotnet/api/documentformat.openxml.spreadsheet.cell?view=openxml-2.8.1
    struct Cell
    {
        // sort cells by location, row first
        bool
        operator<(const Cell& rhs)
        {
            return ref < rhs.ref;
        }

        bool is_phonetic = false; // 'ph'
        xlnt::cell_type type = xlnt::cell_type::number; // 't'
        int cell_metadata_idx = -1; // 'cm'
        int style_index = -1; // 's'
        Cell_Reference ref { 0, 0 }; // 'r'
        std::string value; // <v> OR <is>
        std::string formula_string; // <f>
    };

    // for printing to file.
    // This matches the output format of excel irrespective of current locale
    XLNT_API_INTERNAL std::string
    serialise(double d);

    // Parses a string to a double-precision floating-point number. Optionally,
    // num_characters_parsed can point to a variable where the number of parsed characters will be
    // stored.
    XLNT_API_INTERNAL double
    deserialise(const std::string& s, size_t* num_characters_parsed = nullptr);

    // Parses a string to a double-precision floating-point number. Optionally, end can point
    // to a pointer where the character after the last successfully parsed character will be stored.
    XLNT_API_INTERNAL double
    deserialise(const char* s, const char** end = nullptr);

} // namespace detail
} // namespace xlnt
#endif

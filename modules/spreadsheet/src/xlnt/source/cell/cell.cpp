// Copyright (c) 2014-2022 Thomas Fussell
// Copyright (c) 2010-2015 openpyxl
// Copyright (c) 2024-2026 xlnt-community
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE
//
// @license: http://www.opensource.org/licenses/mit-license.php
// @author: see AUTHORS file

#include <algorithm>
#include <cassert>
#include <cmath>

#include <xlnt/cell/cell.hpp>
#include <xlnt/cell/cell_reference.hpp>
#include <xlnt/cell/comment.hpp>
#include <xlnt/cell/hyperlink.hpp>
#include <xlnt/cell/rich_text.hpp>
#include <xlnt/packaging/manifest.hpp>
#include <xlnt/packaging/relationship.hpp>
#include <xlnt/styles/alignment.hpp>
#include <xlnt/styles/border.hpp>
#include <xlnt/styles/color.hpp>
#include <xlnt/styles/fill.hpp>
#include <xlnt/styles/font.hpp>
#include <xlnt/styles/format.hpp>
#include <xlnt/styles/number_format.hpp>
#include <xlnt/styles/protection.hpp>
#include <xlnt/styles/style.hpp>
#include <xlnt/utils/date.hpp>
#include <xlnt/utils/datetime.hpp>
#include <xlnt/utils/exceptions.hpp>
#include <xlnt/utils/time.hpp>
#include <xlnt/utils/timedelta.hpp>
#include <xlnt/workbook/workbook.hpp>
#include <xlnt/worksheet/column_properties.hpp>
#include <xlnt/worksheet/phonetic_pr.hpp>
#include <xlnt/worksheet/row_properties.hpp>
#include <xlnt/worksheet/worksheet.hpp>
#include <detail/implementations/cell_impl.hpp>
#include <detail/implementations/format_impl.hpp>
#include <detail/implementations/hyperlink_impl.hpp>
#include <detail/implementations/stylesheet.hpp>
#include <detail/implementations/worksheet_impl.hpp>
#include <detail/serialization/serialisation_helpers.hpp>

namespace {

std::pair<bool, double>
cast_numeric(const std::string& s)
{
    size_t len_convert = 0;
    double result = xlnt::detail::deserialise(s, &len_convert);
    return (len_convert != s.size()) ? std::make_pair(false, 0.0) : std::make_pair(true, result);
}

std::pair<bool, double>
cast_percentage(const std::string& s)
{
    if (s.back() == '%') {
        auto number = cast_numeric(s.substr(0, s.size() - 1));

        if (number.first) {
            return { true, number.second / 100 };
        }
    }

    return { false, 0.0 };
}

std::pair<bool, xlnt::time>
cast_time(const std::string& s)
{
    xlnt::time result;

    std::vector<std::string> time_components;
    std::size_t prev = 0;
    auto colon_index = s.find(':');

    while (colon_index != std::string::npos) {
        time_components.push_back(s.substr(prev, colon_index - prev));
        prev = colon_index + 1;
        colon_index = s.find(':', colon_index + 1);
    }

    time_components.push_back(s.substr(prev, colon_index - prev));

    if (time_components.size() < 2 || time_components.size() > 3) {
        return { false, result };
    }

    std::vector<double> numeric_components;

    for (const auto& component : time_components) {
        if (component.empty() || (component.substr(0, component.find('.')).size() > 2)) {
            return { false, result };
        }

        for (auto d : component) {
            if (!(d >= '0' && d <= '9') && d != '.') {
                return { false, result };
            }
        }
        auto numeric = xlnt::detail::deserialise(component);

        numeric_components.push_back(numeric);
    }

    result.hour = static_cast<int>(numeric_components[0]);
    result.minute = static_cast<int>(numeric_components[1]);

    if (std::fabs(static_cast<double>(result.minute) - numeric_components[1])
        > std::numeric_limits<double>::epsilon()) {
        result.minute = result.hour;
        result.hour = 0;
        result.second = static_cast<int>(numeric_components[1]);
        result.microsecond = static_cast<int>((numeric_components[1] - result.second) * 1E6);
    } else if (numeric_components.size() > 2) {
        result.second = static_cast<int>(numeric_components[2]);
        result.microsecond = static_cast<int>((numeric_components[2] - result.second) * 1E6);
    }

    return { true, result };
}

} // namespace

namespace xlnt {

const std::unordered_map<std::string, int>&
cell::error_codes()
{
    static const auto codes
        = std::unordered_map<std::string, int> { { "#NULL!", 0 }, { "#DIV/0!", 1 },
              { "#VALUE!", 2 }, { "#REF!", 3 }, { "#NAME?", 4 }, { "#NUM!", 5 }, { "#N/A!", 6 } };

    return codes;
}

std::string
cell::check_string(const std::string& to_check)
{
    // so we can modify it
    std::string s = to_check;

    if (s.size() == 0) {
        return s;
    } else if (s.size() > 32767) {
        s = s.substr(0, 32767); // max string length in Excel
    }

    for (char c : s) {
        if (c >= 0 && (c <= 8 || c == 11 || c == 12 || (c >= 14 && c <= 31))) {
            throw illegal_character(c);
        }
    }

    return s;
}

cell::cell(detail::cell_impl* d) : d_(d) { }

bool
cell::garbage_collectible() const
{
    return d_->is_garbage_collectible();
}

void
cell::value(std::nullptr_t)
{
    clear_value();
}

void
cell::value(bool boolean_value)
{
    d_->type_ = type::boolean;
    d_->value_numeric_ = boolean_value ? 1.0 : 0.0;
}

void
cell::value(int int_value)
{
    d_->value_numeric_ = static_cast<double>(int_value);
    d_->type_ = type::number;
}

void
cell::value(unsigned int int_value)
{
    d_->value_numeric_ = static_cast<double>(int_value);
    d_->type_ = type::number;
}

void
cell::value(long long int int_value)
{
    d_->value_numeric_ = static_cast<double>(int_value);
    d_->type_ = type::number;
}

void
cell::value(unsigned long long int int_value)
{
    d_->value_numeric_ = static_cast<double>(int_value);
    d_->type_ = type::number;
}

void
cell::value(float float_value)
{
    d_->value_numeric_ = static_cast<double>(float_value);
    d_->type_ = type::number;
}

void
cell::value(double float_value)
{
    d_->value_numeric_ = static_cast<double>(float_value);
    d_->type_ = type::number;
}

void
cell::value(const std::string& s)
{
    value(rich_text(check_string(s)));
}

void
cell::value(const rich_text& text)
{
    check_string(text.plain_text());

    value_no_check(text);
}

void
cell::value(const char* c)
{
    value(std::string(c));
}

void
cell::value(const cell c)
{
    if (c.worksheet().workbook() != worksheet().workbook()) {
        copy_from_other_workbook(c);
        return;
    }

    // Same workbook: shallow copy (existing behavior)
    d_->type_ = c.d_->type_;
    d_->value_numeric_ = c.d_->value_numeric_;
    d_->value_text_ = c.d_->value_text_;
    d_->hyperlink_ = c.d_->hyperlink_;
    d_->formula_ = c.d_->formula_;
    d_->format_ = c.d_->format_;
}

void
cell::value_no_check(const rich_text& text)
{
    d_->type_ = type::shared_string;
    d_->value_numeric_ = static_cast<double>(workbook().add_shared_string(text));
}

void
cell::copy_from_other_workbook(const cell& source)
{
    d_->type_ = source.d_->type_;

    // Handle shared_string: remap to destination workbook
    if (source.data_type() == type::shared_string) {
        value_no_check(source.value<rich_text>());
    } else {
        d_->value_numeric_ = source.d_->value_numeric_;
    }

    d_->value_text_ = source.d_->value_text_;
    d_->formula_ = source.d_->formula_;

    // Copy external hyperlinks; internal hyperlinks (cell/range references)
    // are not yet implemented as they would need worksheet title remapping.
    // TODO: implement internal hyperlink remapping.
    if (source.has_hyperlink()) {
        auto copy_hyperlink = source.hyperlink();

        if (copy_hyperlink.external()) {
            hyperlink(copy_hyperlink.url(), copy_hyperlink.display());
        }
    }

    if (source.has_format()) {
        format(source.format());
    } else {
        d_->format_.clear();
    }
}

void
cell::value(const date& d)
{
    d_->type_ = type::number;
    d_->value_numeric_ = d.to_number(base_date());
    number_format(number_format::date_yyyymmdd2());
}

void
cell::value(const datetime& d)
{
    d_->type_ = type::number;
    d_->value_numeric_ = d.to_number(base_date());
    number_format(number_format::date_datetime());
}

void
cell::value(const time& t)
{
    d_->type_ = type::number;
    d_->value_numeric_ = t.to_number();
    number_format(number_format::date_time6());
}

void
cell::value(const timedelta& t)
{
    d_->type_ = type::number;
    d_->value_numeric_ = t.to_number();
    number_format(xlnt::number_format("[hh]:mm:ss"));
}

row_t
cell::row() const
{
    return d_->row_;
}

column_t
cell::column() const
{
    return d_->column_;
}

column_t::index_t
cell::column_index() const
{
    return d_->column_.index;
}

void
cell::merged(bool merged)
{
    d_->is_merged_ = merged;
}

bool
cell::is_merged() const
{
    return d_->is_merged_;
}

bool
cell::phonetics_visible() const
{
    return d_->phonetics_visible_;
}

void
cell::show_phonetics(bool phonetics)
{
    d_->phonetics_visible_ = phonetics;
}

bool
cell::is_date() const
{
    return data_type() == type::number && has_format() && number_format().is_date_format();
}

cell_reference
cell::reference() const
{
    return { d_->column_, d_->row_ };
}

bool
cell::compare(const cell& other, bool compare_by_reference) const
{
    if (compare_by_reference) {
        return d_ == other.d_;
    } else {
        return *d_ == *other.d_;
    }
}

bool
cell::operator==(const cell& comparand) const
{
    return compare(comparand, true);
}

bool
cell::operator!=(const cell& comparand) const
{
    return !(*this == comparand);
}

cell&
cell::operator=(const cell& rhs)
    = default;

hyperlink
cell::hyperlink() const
{
    if (!d_->hyperlink_.is_set()) {
        throw invalid_attribute("cell \"" + reference().to_string() + "\" has no hyperlink");
    }
    return xlnt::hyperlink(&d_->hyperlink_.get());
}

void
cell::hyperlink(const std::string& url, const std::string& display)
{
    if (url.empty()) {
        throw invalid_parameter(
            "the hyperlink URL for cell \"" + reference().to_string() + "\" is empty");
    }

    auto ws = worksheet();
    auto& manifest = ws.workbook().manifest();

    d_->hyperlink_ = detail::hyperlink_impl();

    // check for existing relationships
    auto relationships = manifest.relationships(ws.path(), relationship_type::hyperlink);
    auto relation = std::find_if(relationships.cbegin(), relationships.cend(),
        [&url](const xlnt::relationship& rel) { return rel.target().to_string() == url; });
    if (relation != relationships.end()) {
        d_->hyperlink_.get().relationship = *relation;
    } else { // register a new relationship
        auto rel_id = manifest.register_relationship(
            uri(ws.path().string()), relationship_type::hyperlink, uri(url), target_mode::external);
        // TODO: make manifest::register_relationship return the created relationship instead of rel
        // id
        d_->hyperlink_.get().relationship = manifest.relationship(ws.path(), rel_id);
    }
    // if a value is already present, the display string is ignored
    if (has_value()) {
        d_->hyperlink_.get().display.set(to_string());
    } else {
        d_->hyperlink_.get().display.set(display.empty() ? url : display);
        value(hyperlink().display());
    }
}

void
cell::hyperlink(xlnt::cell target, const std::string& display)
{
    // TODO: should this computed value be a method on a cell?
    const auto cell_address = target.worksheet().title() + "!" + target.reference().to_string();

    d_->hyperlink_ = detail::hyperlink_impl();
    d_->hyperlink_.get().relationship = xlnt::relationship(
        "", relationship_type::hyperlink, uri(""), uri(cell_address), target_mode::internal);
    // if a value is already present, the display string is ignored
    if (has_value()) {
        d_->hyperlink_.get().display.set(to_string());
    } else {
        d_->hyperlink_.get().display.set(display.empty() ? cell_address : display);
        value(hyperlink().display());
    }
}

void
cell::hyperlink(xlnt::range target, const std::string& display)
{
    // TODO: should this computed value be a method on a cell?
    const auto range_address
        = target.target_worksheet().title() + "!" + target.reference().to_string();

    d_->hyperlink_ = detail::hyperlink_impl();
    d_->hyperlink_.get().relationship = xlnt::relationship(
        "", relationship_type::hyperlink, uri(""), uri(range_address), target_mode::internal);

    // if a value is already present, the display string is ignored
    if (has_value()) {
        d_->hyperlink_.get().display.set(to_string());
    } else {
        d_->hyperlink_.get().display.set(display.empty() ? range_address : display);
        value(hyperlink().display());
    }
}

void
cell::formula(const std::string& formula)
{
    if (formula.empty()) {
        return clear_formula();
    }

    if (formula[0] == '=') {
        d_->formula_ = formula.substr(1);
    } else {
        d_->formula_ = formula;
    }

    worksheet().register_calc_chain_in_manifest();
}

bool
cell::has_formula() const
{
    return d_->formula_.is_set();
}

std::string
cell::formula() const
{
    if (!d_->formula_.is_set()) {
        throw invalid_attribute("cell \"" + reference().to_string() + "\" has no formula");
    }
    return d_->formula_.get();
}

void
cell::clear_formula()
{
    if (has_formula()) {
        d_->formula_.clear();
        worksheet().garbage_collect_formulae();
    }
}

std::string
cell::error() const
{
    if (d_->type_ != type::error) {
        throw xlnt::invalid_attribute("called error() when cell type is not error, but "
            + std::to_string(static_cast<int>(d_->type_)));
    }
    return value<std::string>();
}

void
cell::error(const std::string& error)
{
    if (error.length() == 0 || error[0] != '#') {
        throw invalid_data_type(error);
    }

    d_->value_text_.plain_text(error, false);
    d_->type_ = type::error;
}

cell
cell::offset(int column, int row)
{
    return worksheet().cell(reference().make_offset(column, row));
}

worksheet
cell::worksheet()
{
    return xlnt::worksheet(d_->parent_);
}

const worksheet
cell::worksheet() const
{
    return xlnt::worksheet(d_->parent_);
}

workbook
cell::workbook()
{
    return worksheet().workbook();
}

const workbook
cell::workbook() const
{
    return worksheet().workbook();
}

std::pair<int, int>
cell::anchor() const
{
    double left = 0;

    for (column_t column_index = 1; column_index <= d_->column_ - 1; column_index++) {
        left += worksheet().column_width(column_index);
    }

    double top = 0;

    for (row_t row_index = 1; row_index <= d_->row_ - 1; row_index++) {
        top += worksheet().row_height(row_index);
    }

    return { static_cast<int>(left), static_cast<int>(top) };
}

cell::type
cell::data_type() const
{
    return d_->type_;
}

void
cell::data_type(type t)
{
    d_->type_ = t;
}

number_format
cell::computed_number_format() const
{
    return xlnt::number_format();
}

font
cell::computed_font() const
{
    return xlnt::font();
}

fill
cell::computed_fill() const
{
    return xlnt::fill();
}

border
cell::computed_border() const
{
    return xlnt::border();
}

alignment
cell::computed_alignment() const
{
    return xlnt::alignment();
}

protection
cell::computed_protection() const
{
    return xlnt::protection();
}

void
cell::clear_value()
{
    d_->value_numeric_ = 0;
    d_->value_text_.clear();
    d_->type_ = cell::type::empty;
    clear_formula();
}

template <>
bool
cell::value() const
{
    return d_->value_numeric_ != 0.0;
}

template <>
int
cell::value() const
{
    return static_cast<int>(d_->value_numeric_);
}

template <>
long long int
cell::value() const
{
    return static_cast<long long int>(d_->value_numeric_);
}

template <>
unsigned int
cell::value() const
{
    return static_cast<unsigned int>(d_->value_numeric_);
}

template <>
unsigned long long
cell::value() const
{
    return static_cast<unsigned long long>(d_->value_numeric_);
}

template <>
float
cell::value() const
{
    return static_cast<float>(d_->value_numeric_);
}

template <>
double
cell::value() const
{
    return static_cast<double>(d_->value_numeric_);
}

template <>
time
cell::value() const
{
    return time::from_number(d_->value_numeric_);
}

template <>
datetime
cell::value() const
{
    return datetime::from_number(d_->value_numeric_, base_date());
}

template <>
date
cell::value() const
{
    return date::from_number(static_cast<int>(d_->value_numeric_), base_date());
}

template <>
timedelta
cell::value() const
{
    return timedelta::from_number(d_->value_numeric_);
}

void
cell::alignment(const class alignment& alignment_)
{
    auto new_format = has_format() ? modifiable_format() : workbook().create_format();
    format(new_format.alignment(alignment_, optional<bool>(true)));
}

void
cell::border(const class border& border_)
{
    auto new_format = has_format() ? modifiable_format() : workbook().create_format();
    format(new_format.border(border_, optional<bool>(true)));
}

void
cell::fill(const class fill& fill_)
{
    auto new_format = has_format() ? modifiable_format() : workbook().create_format();
    format(new_format.fill(fill_, optional<bool>(true)));
}

void
cell::font(const class font& font_)
{
    auto new_format = has_format() ? modifiable_format() : workbook().create_format();
    format(new_format.font(font_, optional<bool>(true)));
}

void
cell::number_format(const class number_format& number_format_)
{
    auto new_format = has_format() ? modifiable_format() : workbook().create_format();
    format(new_format.number_format(number_format_, optional<bool>(true)));
}

void
cell::protection(const class protection& protection_)
{
    auto new_format = has_format() ? modifiable_format() : workbook().create_format();
    format(new_format.protection(protection_, optional<bool>(true)));
}

template <>
std::string
cell::value() const
{
    return value<rich_text>().plain_text();
}

template <>
rich_text
cell::value() const
{
    if (data_type() == cell::type::shared_string) {
        return workbook().shared_strings(static_cast<std::size_t>(d_->value_numeric_));
    }

    return d_->value_text_;
}

bool
cell::has_value() const
{
    return d_->type_ != cell::type::empty;
}

std::string
cell::to_string() const
{
    auto nf = computed_number_format();

    switch (data_type()) {
    case cell::type::empty:
        return "";
    case cell::type::date:
    case cell::type::number:
        return nf.format(value<double>(), base_date());
    case cell::type::inline_string:
    case cell::type::shared_string:
    case cell::type::formula_string:
    case cell::type::error:
        return nf.format(value<std::string>());
    case cell::type::boolean:
        return value<double>() == 0.0 ? "FALSE" : "TRUE";
    }

    return "";
}

bool
cell::has_format() const
{
    return d_->format_.is_set();
}

void
cell::format(const class format new_format)
{
    // Check if format belongs to a different workbook (dangling pointer risk)
    if (!workbook().owns_format(new_format)) {
        copy_format_from_other_workbook(new_format);
        return;
    }

    // Same workbook: direct assignment (original behavior)
    d_->format_ = new_format.d_;
}

void
cell::copy_format_from_other_workbook(const class format& source_format)
{
    auto cloned_format = workbook().clone_format_from(source_format);

    // Use the cloned format
    d_->format_ = cloned_format.d_;
}

calendar
cell::base_date() const
{
    return workbook().base_date();
}

bool
operator==(std::nullptr_t, const cell& cell)
{
    return cell.data_type() == cell::type::empty;
}

bool
operator==(const cell& cell, std::nullptr_t)
{
    return nullptr == cell;
}

bool
operator!=(std::nullptr_t, const cell& cell)
{
    return !(nullptr == cell);
}

bool
operator!=(const cell& cell, std::nullptr_t)
{
    return nullptr != cell;
}

std::ostream&
operator<<(std::ostream& stream, const xlnt::cell& cell)
{
    return stream << cell.to_string();
}

void
cell::value(const std::string& value_string, bool infer_type)
{
    value(value_string);

    if (!infer_type || value_string.empty()) {
        return;
    }

    if (value_string.front() == '=' && value_string.size() > 1) {
        formula(value_string);
        return;
    }

    if (value_string.front() == '#' && value_string.size() > 1) {
        error(value_string);
        return;
    }

    auto percentage = cast_percentage(value_string);

    if (percentage.first) {
        d_->value_numeric_ = percentage.second;
        d_->type_ = cell::type::number;
        number_format(xlnt::number_format::percentage());
    } else {
        auto time = cast_time(value_string);

        if (time.first) {
            d_->type_ = cell::type::number;
            number_format(number_format::date_time6());
            d_->value_numeric_ = time.second.to_number();
        } else {
            auto numeric = cast_numeric(value_string);

            if (numeric.first) {
                d_->value_numeric_ = numeric.second;
                d_->type_ = cell::type::number;
            }
        }
    }
}

void
cell::clear_format()
{
    if (d_->format_.is_set()) {
        d_->format_.clear();
    }
}

void
cell::clear_style()
{
    if (has_format()) {
        modifiable_format().clear_style();
    }
}

void
cell::style(const class style& new_style)
{
    auto new_format = has_format() ? format() : workbook().create_format();

    new_format.alignment(new_style.alignment());
    new_format.border(new_style.border());
    new_format.fill(new_style.fill());
    new_format.font(new_style.font());
    new_format.number_format(new_style.number_format());
    new_format.protection(new_style.protection());

    format(new_format.style(new_style));
}

void
cell::style(const std::string& style_name)
{
    style(workbook().style(style_name));
}

style
cell::style()
{
    if (!has_format() || !format().has_style()) {
        throw invalid_attribute("cell " + reference().to_string() + " does not have a style");
    }

    auto f = format();

    return f.style();
}

const style
cell::style() const
{
    if (!has_format() || !format().has_style()) {
        throw invalid_attribute("cell " + reference().to_string() + " does not have a style");
    }

    return format().style();
}

bool
cell::has_style() const
{
    return has_format() && format().has_style();
}

format
cell::modifiable_format()
{
    if (!d_->format_.is_set()) {
        throw invalid_attribute("cell " + reference().to_string() + " does not have a format");
    }

    return xlnt::format(d_->format_);
}

const format
cell::format() const
{
    if (!d_->format_.is_set()) {
        throw invalid_attribute("cell " + reference().to_string() + " does not have a format");
    }

    return xlnt::format(d_->format_);
}

alignment
cell::alignment() const
{
    if (has_format()) {
        return format().alignment();
    }

    return {};
}

border
cell::border() const
{
    if (has_format()) {
        return format().border();
    }

    return {};
}

fill
cell::fill() const
{
    if (has_format()) {
        return format().fill();
    }

    return {};
}

font
cell::font() const
{
    if (has_format()) {
        return format().font();
    }

    return {};
}

number_format
cell::number_format() const
{
    if (has_format()) {
        return format().number_format();
    }

    return {};
}

protection
cell::protection() const
{
    if (has_format()) {
        return format().protection();
    }

    return {};
}

bool
cell::has_hyperlink() const
{
    return d_->hyperlink_.is_set();
}

// comment

bool
cell::has_comment() const
{
    return d_->comment_.is_set();
}

void
cell::clear_comment()
{
    if (has_comment()) {
        d_->parent_->comments_.erase(reference().to_string());
        d_->comment_.clear();
    }
}

class comment
cell::comment() const
{
    if (!has_comment()) {
        throw xlnt::invalid_attribute("cell " + reference().to_string() + " has no comment");
    }

    return *d_->comment_.get();
}

void
cell::comment(const std::string& text, const std::string& author)
{
    comment(xlnt::comment(text, author));
}

void
cell::comment(const std::string& text, const class font& comment_font, const std::string& author)
{
    comment(xlnt::comment(xlnt::rich_text(text, comment_font), author));
}

void
cell::comment(const class comment& new_comment)
{
    if (has_comment()) {
        *d_->comment_.get() = new_comment;
    } else {
        d_->parent_->comments_[reference().to_string()] = new_comment;
        d_->comment_.set(&d_->parent_->comments_[reference().to_string()]);
    }

    // offset comment 5 pixels down and 5 pixels right of the top right corner of the cell
    auto cell_position = anchor();
    cell_position.first += static_cast<int>(width()) + 5;
    cell_position.second += 5;

    d_->comment_.get()->position(cell_position.first, cell_position.second);

    worksheet().register_comments_in_manifest();
}

double
cell::width() const
{
    return worksheet().column_width(column());
}

double
cell::height() const
{
    return worksheet().row_height(row());
}

} // namespace xlnt

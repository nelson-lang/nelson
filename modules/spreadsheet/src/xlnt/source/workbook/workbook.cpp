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
#include <fstream>
#include <functional>

#include <xlnt/cell/cell.hpp>
#include <xlnt/packaging/manifest.hpp>
#include <xlnt/packaging/relationship.hpp>
#include <xlnt/styles/alignment.hpp>
#include <xlnt/styles/border.hpp>
#include <xlnt/styles/fill.hpp>
#include <xlnt/styles/font.hpp>
#include <xlnt/styles/format.hpp>
#include <xlnt/styles/number_format.hpp>
#include <xlnt/styles/protection.hpp>
#include <xlnt/styles/style.hpp>
#include <xlnt/utils/exceptions.hpp>
#include <xlnt/utils/path.hpp>
#include <xlnt/utils/variant.hpp>
#include <xlnt/workbook/metadata_property.hpp>
#include <xlnt/workbook/named_range.hpp>
#include <xlnt/workbook/theme.hpp>
#include <xlnt/workbook/workbook.hpp>
#include <xlnt/workbook/workbook_view.hpp>
#include <xlnt/workbook/worksheet_iterator.hpp>
#include <xlnt/worksheet/header_footer.hpp>
#include <xlnt/worksheet/range.hpp>
#include <xlnt/worksheet/worksheet.hpp>
#include <detail/constants.hpp>
#include <detail/default_case.hpp>
#include <detail/implementations/cell_impl.hpp>
#include <detail/implementations/workbook_impl.hpp>
#include <detail/implementations/worksheet_impl.hpp>
#include <detail/serialization/excel_thumbnail.hpp>
#include <detail/serialization/open_stream.hpp>
#include <detail/serialization/parsers.hpp>
#include <detail/serialization/vector_streambuf.hpp>
#include <detail/serialization/xlsx_consumer.hpp>
#include <detail/serialization/xlsx_producer.hpp>

namespace {

using xlnt::detail::open_stream;

template <typename T>
std::vector<T>
keys(const std::vector<std::pair<T, xlnt::variant>>& container)
{
    auto result = std::vector<T>();
    result.reserve(container.size());
    auto iter = container.begin();

    while (iter != container.end()) {
        result.push_back((iter++)->first);
    }

    return result;
}

template <typename T>
bool
contains(const std::vector<std::pair<T, xlnt::variant>>& container, const T key)
{
    for (const auto& iter : container) {
        if (iter.first == key) {
            return true;
        }
    }

    return false;
}

xlnt::path
default_path(xlnt::relationship_type type, std::size_t index = 0)
{
    using xlnt::path;
    using xlnt::relationship_type;

    switch (type) {
    case relationship_type::calculation_chain:
        return path("/xl/calcChain.xml");
    case relationship_type::chartsheet:
        return path("/xl/sheets/.xml");
    case relationship_type::comments:
        return path("/xl/comments.xml");
    case relationship_type::connections:
        return path("/xl/connections.xml");
    case relationship_type::core_properties:
        return path("/docProps/core.xml");
    case relationship_type::custom_properties:
        return path("/docProps/custom.xml");
    case relationship_type::custom_property:
        return path("/xl/customProperty.xml");
    case relationship_type::custom_xml_mappings:
        return path("/xl/customXmlMappings.xml");
    case relationship_type::dialogsheet:
        return path("/xl/dialogsheets/sheet.xml");
    case relationship_type::drawings:
        return path("/xl/drawings/drawing.xml");
    case relationship_type::extended_properties:
        return path("/docProps/app.xml");
    case relationship_type::external_workbook_references:
        return path("/xl/external.xml");
    case relationship_type::hyperlink:
        return path("/xl/hyperlink.xml");
    case relationship_type::image:
        return path("?");
    case relationship_type::office_document:
        return path("/xl/workbook.xml");
    case relationship_type::pivot_table:
        return path("/xl/pivotTable.xml");
    case relationship_type::pivot_table_cache_definition:
        return path("?");
    case relationship_type::pivot_table_cache_records:
        return path("?");
    case relationship_type::printer_settings:
        return path("/xl/printerSettings.xml");
    case relationship_type::query_table:
        return path("/xl/queryTable.xml");
    case relationship_type::revision_log:
        return path("/xl/revisionLog.xml");
    case relationship_type::shared_string_table:
        return path("/xl/sharedStrings.xml");
    case relationship_type::shared_workbook:
        return path("/xl/sharedWorkbook.xml");
    case relationship_type::shared_workbook_revision_headers:
        return path("?");
    case relationship_type::shared_workbook_user_data:
        return path("?");
    case relationship_type::single_cell_table_definitions:
        return path("?");
    case relationship_type::stylesheet:
        return path("/xl/styles.xml");
    case relationship_type::table_definition:
        return path("/xl/tableDefinition.xml");
    case relationship_type::theme:
        return path("/xl/theme/theme1.xml");
    case relationship_type::thumbnail:
        return path("/docProps/thumbnail.jpg");
    case relationship_type::unknown:
        return path("/xl/unknown.xml");
    case relationship_type::vml_drawing:
        return path("/xl/vmlDrawing.xml");
    case relationship_type::volatile_dependencies:
        return path("/xl/volatileDependencies.xml");
    case relationship_type::vbaproject:
        return path("/xl/vbaProject.bin");
    case relationship_type::worksheet:
        return path("/xl/worksheets/sheet" + std::to_string(index) + ".xml");
    }

    default_case(path("/xl/unknownPart.xml"));
}

std::string
content_type(xlnt::relationship_type type)
{
    using xlnt::relationship_type;

    switch (type) {
    case relationship_type::calculation_chain:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.calcChain+xml";
    case relationship_type::chartsheet:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml";
    case relationship_type::comments:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.comments+xml";
    case relationship_type::connections:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.connections+xml";
    case relationship_type::core_properties:
        return "application/vnd.openxmlformats-package.core-properties+xml";
    case relationship_type::custom_properties:
        return "application/vnd.openxmlformats-officedocument.custom-properties+xml";
    case relationship_type::custom_property:
        throw xlnt::unhandled_switch_case("xlnt::relationship_type::custom_property");
    case relationship_type::custom_xml_mappings:
        return "application/xml";
    case relationship_type::dialogsheet:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml";
    case relationship_type::drawings:
        return "application/vnd.openxmlformats-officedocument.drawing+xml";
    case relationship_type::extended_properties:
        return "application/vnd.openxmlformats-officedocument.extended-properties+xml";
    case relationship_type::external_workbook_references:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.externalLink+xml";
    case relationship_type::hyperlink:
        throw xlnt::unhandled_switch_case("xlnt::relationship_type::hyperlink");
    case relationship_type::image:
        throw xlnt::unhandled_switch_case("xlnt::relationship_type::image");
    case relationship_type::office_document:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml";
    case relationship_type::pivot_table:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.pivotTable+xml";
    case relationship_type::pivot_table_cache_definition:
        return "application/"
               "vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheDefinition+xml";
    case relationship_type::pivot_table_cache_records:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheRecords+xml";
    case relationship_type::printer_settings:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.printerSettings";
    case relationship_type::query_table:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.queryTable+xml";
    case relationship_type::revision_log:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.revisionLog+xml";
    case relationship_type::shared_string_table:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml";
    case relationship_type::shared_workbook:
        throw xlnt::unhandled_switch_case("xlnt::relationship_type::shared_workbook");
    case relationship_type::shared_workbook_revision_headers:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.revisionHeaders+xml";
    case relationship_type::shared_workbook_user_data:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.userNames+xml";
    case relationship_type::single_cell_table_definitions:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.tableSingleCells+xml";
    case relationship_type::stylesheet:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml";
    case relationship_type::table_definition:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.table+xml";
    case relationship_type::theme:
        return "application/vnd.openxmlformats-officedocument.theme+xml";
    case relationship_type::thumbnail:
        return "image/jpeg";
    case relationship_type::unknown:
        return "";
    case relationship_type::vml_drawing:
        return "application/vnd.openxmlformats-officedocument.vmlDrawing";
    case relationship_type::volatile_dependencies:
        return "application/"
               "vnd.openxmlformats-officedocument.spreadsheetml.volatileDependencies+xml";
    case relationship_type::vbaproject:
        return "application/vnd.ms-office.vbaProject";
    case relationship_type::worksheet:
        return "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml";
    }

    default_case("application/xml");
}

} // namespace

namespace xlnt {

bool
workbook::has_core_property(xlnt::core_property type) const
{
    return ::contains(d_->core_properties_, type);
}

std::vector<xlnt::core_property>
workbook::core_properties() const
{
    return keys(d_->core_properties_);
}

variant
workbook::core_property(xlnt::core_property type) const
{
    for (auto iter : d_->core_properties_) {
        if (iter.first == type) {
            return iter.second;
        }
    }

    return {};
}

void
workbook::core_property(xlnt::core_property type, const variant& value)
{
    register_package_part(relationship_type::core_properties);

    for (auto& iter : d_->core_properties_) {
        if (iter.first == type) {
            iter.second = value;
            return;
        }
    }

    d_->core_properties_.push_back({ type, value });
}

bool
workbook::has_extended_property(xlnt::extended_property type) const
{
    return ::contains(d_->extended_properties_, type);
}

std::vector<xlnt::extended_property>
workbook::extended_properties() const
{
    return keys(d_->extended_properties_);
}

void
workbook::extended_property(xlnt::extended_property type, const variant& value)
{
    register_package_part(relationship_type::extended_properties);

    for (auto& iter : d_->extended_properties_) {
        if (iter.first == type) {
            iter.second = value;
            return;
        }
    }

    d_->extended_properties_.push_back({ type, value });
}

variant
workbook::extended_property(xlnt::extended_property type) const
{
    for (auto iter : d_->extended_properties_) {
        if (iter.first == type) {
            return iter.second;
        }
    }

    return {};
}

bool
workbook::has_custom_property(const std::string& property_name) const
{
    return ::contains(d_->custom_properties_, property_name);
}

std::vector<std::string>
workbook::custom_properties() const
{
    return keys(d_->custom_properties_);
}

void
workbook::custom_property(const std::string& property_name, const variant& value)
{
    register_package_part(relationship_type::custom_properties);

    for (auto& iter : d_->custom_properties_) {
        if (iter.first == property_name) {
            iter.second = value;
            return;
        }
    }

    d_->custom_properties_.push_back({ property_name, value });
}

variant
workbook::custom_property(const std::string& property_name) const
{
    for (auto iter : d_->custom_properties_) {
        if (iter.first == property_name) {
            return iter.second;
        }
    }

    return {};
}

void
workbook::abs_path(const std::string& path)
{
    d_->abs_path_ = path;
}

void
workbook::arch_id_flags(const std::size_t flags)
{
    d_->arch_id_flags_ = flags;
}

workbook
workbook::empty()
{
    workbook wb(std::make_shared<detail::workbook_impl>());

    wb.register_package_part(relationship_type::office_document);

    wb.d_->manifest_.register_default_type(
        "rels", "application/vnd.openxmlformats-package.relationships+xml");
    wb.d_->manifest_.register_default_type("xml", "application/xml");

    wb.thumbnail(excel_thumbnail(), "jpeg", "image/jpeg");

    wb.core_property(xlnt::core_property::creator, "Microsoft Office User");
    wb.core_property(xlnt::core_property::last_modified_by, "Microsoft Office User");
    wb.core_property(xlnt::core_property::created, datetime(2016, 8, 12, 3, 16, 56));
    wb.core_property(xlnt::core_property::modified, datetime(2016, 8, 12, 3, 17, 16));

    wb.extended_property(xlnt::extended_property::application, "Microsoft Macintosh Excel");
    wb.extended_property(xlnt::extended_property::doc_security, 0);
    wb.extended_property(xlnt::extended_property::scale_crop, false);
    wb.extended_property(xlnt::extended_property::heading_pairs,
        std::vector<variant> { variant("Worksheets"), variant(1) });
    wb.extended_property(xlnt::extended_property::titles_of_parts, { "Sheet1" });
    wb.extended_property(xlnt::extended_property::company, "");
    wb.extended_property(xlnt::extended_property::links_up_to_date, false);
    wb.extended_property(xlnt::extended_property::shared_doc, false);
    wb.extended_property(xlnt::extended_property::hyperlinks_changed, false);
    wb.extended_property(xlnt::extended_property::app_version, "15.0300");

    wb.d_->file_version_ = detail::workbook_impl::file_version_t();
    auto& file_version = wb.d_->file_version_.get();
    file_version.app_name = "xl";
    file_version.last_edited = "6";
    file_version.lowest_edited = "6";
    file_version.rup_build = "26709";

    xlnt::workbook_view wb_view;
    wb_view.active_tab = 0;
    wb_view.x_window = 0;
    wb_view.y_window = 460;
    wb_view.window_width = 28800;
    wb_view.window_height = 17460;
    wb_view.tab_ratio = 500;
    wb.view(wb_view);

    auto ws = wb.create_sheet();

    page_margins margins;
    margins.left(0.7);
    margins.right(0.7);
    margins.top(0.75);
    margins.bottom(0.75);
    margins.header(0.3);
    margins.footer(0.3);
    ws.page_margins(margins);

    sheet_view view;
    ws.add_view(view);

    auto& format_properties = ws.d_->format_properties_;
    format_properties.base_col_width = 10.0;
    format_properties.default_row_height = 16.0;

    wb.theme(xlnt::theme());

    wb.d_->stylesheet_ = detail::stylesheet();
    auto& stylesheet = wb.d_->stylesheet_.get();
    stylesheet.parent = wb.d_;

    auto default_border = border()
                              .side(border_side::bottom, border::border_property())
                              .side(border_side::top, border::border_property())
                              .side(border_side::start, border::border_property())
                              .side(border_side::end, border::border_property())
                              .side(border_side::diagonal, border::border_property());
    wb.d_->stylesheet_.get().borders.push_back(default_border);

    auto default_fill = fill(pattern_fill().type(pattern_fill_type::none));
    stylesheet.fills.push_back(default_fill);
    auto gray125_fill = pattern_fill().type(pattern_fill_type::gray125);
    stylesheet.fills.push_back(gray125_fill);

    auto default_font
        = font().name("Calibri").size(12).scheme("minor").family(2).color(theme_color(1));
    stylesheet.fonts.push_back(default_font);

    wb.create_builtin_style(0)
        .border(default_border)
        .fill(default_fill)
        .font(default_font)
        .number_format(xlnt::number_format::general());

    auto format = wb.create_format()
                      .border(default_border)
                      .fill(default_fill)
                      .font(default_font)
                      .number_format(xlnt::number_format::general())
                      .style("Normal");
    wb.default_format(format);

    xlnt::calculation_properties calc_props;
    calc_props.calc_id = 150000;
    wb.calculation_properties(calc_props);

    return wb;
}

workbook::workbook()
{
    auto wb_template = empty();
    swap(wb_template);
}

workbook::workbook(const xlnt::path& file)
{
    *this = empty();
    load(file);
}

template <typename T>
void
workbook::construct(const xlnt::path& file, const T& password)
{
    *this = empty();
    load(file, password);
}

workbook::workbook(const xlnt::path& file, const std::string& password)
{
    construct(file, password);
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
workbook::workbook(const xlnt::path& file, std::u8string_view password)
{
    construct(file, password);
}
#endif

workbook::workbook(std::istream& data)
{
    *this = empty();
    load(data);
}

template <typename T>
void
workbook::construct(std::istream& data, const T& password)
{
    *this = empty();
    load(data, password);
}

workbook::workbook(std::istream& data, const std::string& password) { construct(data, password); }

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
workbook::workbook(std::istream& data, std::u8string_view password) { construct(data, password); }
#endif

workbook::workbook(std::shared_ptr<detail::workbook_impl> impl) { set_impl(std::move(impl)); }

workbook::workbook(std::weak_ptr<detail::workbook_impl> impl) { set_impl(impl.lock()); }

void
workbook::set_impl(std::shared_ptr<detail::workbook_impl> impl)
{
    if (impl == nullptr) {
        throw xlnt::invalid_parameter("invalid workbook pointer");
    }

    d_ = std::move(impl);
}

void
workbook::register_package_part(relationship_type type)
{
    if (!manifest().has_relationship(path("/"), type)) {
        manifest().register_override_type(default_path(type), content_type(type));
        manifest().register_relationship(uri("/"), type,
            uri(default_path(type).relative_to(path("/")).string()), target_mode::internal);
    }
}

void
workbook::register_workbook_part(relationship_type type)
{
    auto wb_rel = manifest().relationship(path("/"), relationship_type::office_document);
    auto wb_path = manifest().canonicalize({ wb_rel });

    if (!manifest().has_relationship(wb_path, type)) {
        manifest().register_override_type(default_path(type), content_type(type));
        manifest().register_relationship(uri(wb_path.string()), type,
            uri(default_path(type).relative_to(wb_path.resolve(path("/"))).string()),
            target_mode::internal);
    }
}

void
workbook::register_worksheet_part(worksheet ws, relationship_type type)
{
    auto wb_rel = manifest().relationship(path("/"), relationship_type::office_document);
    auto ws_rel = manifest().relationship(
        wb_rel.target().path(), d_->sheet_title_rel_id_map_.at(ws.title()));
    path ws_path(ws_rel.source().path().parent().append(ws_rel.target().path()));

    if (type == relationship_type::comments) {
        if (!manifest().has_relationship(ws_path, relationship_type::vml_drawing)) {
            std::size_t file_number = 1;
            path filename("vmlDrawing1.vml");
            bool filename_exists = true;

            while (filename_exists) {
                filename_exists = false;

                for (auto current_ws_rel : manifest().relationships(
                         wb_rel.target().path(), xlnt::relationship_type::worksheet)) {
                    path current_ws_path(current_ws_rel.source().path().parent().append(
                        current_ws_rel.target().path()));
                    if (!manifest().has_relationship(
                            current_ws_path, xlnt::relationship_type::vml_drawing)) {
                        continue;
                    }

                    for (auto current_ws_child_rel : manifest().relationships(
                             current_ws_path, xlnt::relationship_type::vml_drawing)) {
                        if (current_ws_child_rel.target().path()
                            == path("../drawings").append(filename)) {
                            filename_exists = true;
                            break;
                        }
                    }
                }

                if (filename_exists) {
                    file_number++;
                    filename = path("vmlDrawing" + std::to_string(file_number) + ".vml");
                }
            }

            manifest().register_default_type(
                "vml", "application/vnd.openxmlformats-officedocument.vmlDrawing");

            const path relative_path(path("../drawings").append(filename));
            manifest().register_relationship(uri(ws_path.string()), relationship_type::vml_drawing,
                uri(relative_path.string()), target_mode::internal);
        }

        if (!manifest().has_relationship(ws_path, relationship_type::comments)) {
            std::size_t file_number = 1;
            path filename("comments1.xml");

            while (true) {
                if (!manifest().has_override_type(constants::package_xl().append(filename))) {
                    break;
                }

                file_number++;
                filename = path("comments" + std::to_string(file_number) + ".xml");
            }

            const path absolute_path(constants::package_xl().append(filename));
            manifest().register_override_type(absolute_path,
                "application/vnd.openxmlformats-officedocument.spreadsheetml.comments+xml");

            const path relative_path(path("..").append(filename));
            manifest().register_relationship(uri(ws_path.string()), relationship_type::comments,
                uri(relative_path.string()), target_mode::internal);
        }
    }
}

const worksheet
workbook::sheet_by_title(const std::string& title) const
{
    for (auto& impl : d_->worksheets_) {
        if (impl.title_ == title) {
            return worksheet(&impl);
        }
    }

    throw key_not_found(title);
}

worksheet
workbook::sheet_by_title(const std::string& title)
{
    for (auto& impl : d_->worksheets_) {
        if (impl.title_ == title) {
            return worksheet(&impl);
        }
    }

    throw key_not_found(title);
}

worksheet
workbook::sheet_by_index(std::size_t index)
{
    if (index >= d_->worksheets_.size()) {
        throw invalid_parameter("invalid worksheet index " + std::to_string(index)
            + " (workbook contains " + std::to_string(d_->worksheets_.size()) + " worksheets)");
    }

    auto iter = d_->worksheets_.begin();

    for (std::size_t i = 0; i < index; ++i) {
        ++iter;
    }

    return worksheet(&*iter);
}

const worksheet
workbook::sheet_by_index(std::size_t index) const
{
    if (index >= d_->worksheets_.size()) {
        throw invalid_parameter("invalid worksheet index " + std::to_string(index)
            + " (workbook contains " + std::to_string(d_->worksheets_.size()) + " worksheets)");
    }

    auto iter = d_->worksheets_.begin();

    for (std::size_t i = 0; i < index; ++i, ++iter) { }

    return worksheet(&*iter);
}

bool
workbook::has_sheet_id(std::size_t id) const
{
    for (auto& impl : d_->worksheets_) {
        if (impl.id_ == id) {
            return true;
        }
    }

    return false;
}

worksheet
workbook::sheet_by_id(std::size_t id)
{
    for (auto& impl : d_->worksheets_) {
        if (impl.id_ == id) {
            return worksheet(&impl);
        }
    }

    throw key_not_found(std::to_string(id));
}

const worksheet
workbook::sheet_by_id(std::size_t id) const
{
    for (auto& impl : d_->worksheets_) {
        if (impl.id_ == id) {
            return worksheet(&impl);
        }
    }

    throw key_not_found(std::to_string(id));
}

bool
workbook::sheet_hidden_by_index(std::size_t index) const
{
    if (index >= d_->sheet_hidden_.size()) {
        throw invalid_parameter("invalid hidden worksheet index " + std::to_string(index)
            + " (workbook contains " + std::to_string(d_->sheet_hidden_.size())
            + " hidden worksheets)");
    }

    return d_->sheet_hidden_.at(index);
}

worksheet
workbook::active_sheet()
{
    return sheet_by_index(d_->active_sheet_index_.is_set() ? d_->active_sheet_index_.get() : 0);
}
void
workbook::active_sheet(std::size_t index)
{
    d_->active_sheet_index_.set(index);
    d_->view_.get().active_tab = index;
}

bool
workbook::has_named_range(const std::string& name) const
{
    for (auto worksheet : *this) {
        if (worksheet.has_named_range(name)) {
            return true;
        }
    }
    return false;
}

worksheet
workbook::create_sheet()
{
    std::string title = "Sheet1";
    int index = 1;

    // make a unique sheet name. Sheet<1...n>
    while (contains(title)) {
        title = "Sheet" + std::to_string(++index);
    }
    // unique sheet id
    size_t sheet_id = 1;
    for (const auto ws : *this) {
        sheet_id = std::max(sheet_id, ws.id() + 1);
    }
    d_->worksheets_.push_back(detail::worksheet_impl(this, sheet_id, title));
    // unique sheet file name
    auto workbook_rel = d_->manifest_.relationship(path("/"), relationship_type::office_document);
    auto workbook_files = d_->manifest_.relationships(workbook_rel.target().path());
    auto rel_vec_contains = [&workbook_files](const xlnt::path& new_file_id) {
        return workbook_files.end()
            != std::find_if(workbook_files.begin(), workbook_files.end(),
                [&new_file_id](
                    const xlnt::relationship& rel) { return rel.target().path() == new_file_id; });
    };

    size_t file_id = sheet_id;
    xlnt::path sheet_relative_path;
    do {
        sheet_relative_path
            = path("worksheets").append("sheet" + std::to_string(file_id++) + ".xml");
    } while (rel_vec_contains(sheet_relative_path));

    uri relative_sheet_uri(sheet_relative_path.string());
    auto absolute_sheet_path = path("/xl").append(relative_sheet_uri.path());
    d_->manifest_.register_override_type(absolute_sheet_path,
        "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml");
    auto ws_rel = d_->manifest_.register_relationship(workbook_rel.target(),
        relationship_type::worksheet, relative_sheet_uri, target_mode::internal);
    d_->sheet_title_rel_id_map_[title] = ws_rel;

    update_sheet_properties();
    reorder_relationships();

    return worksheet(&d_->worksheets_.back());
}

worksheet
workbook::copy_sheet(worksheet to_copy)
{
    if (to_copy.d_->parent_.lock() != d_) {
        throw invalid_parameter("the worksheets do not have the same parent workbook");
    }

    detail::worksheet_impl impl(*to_copy.d_);
    auto new_sheet = create_sheet();
    impl.title_ = new_sheet.title();
    impl.id_ = new_sheet.id();
    *new_sheet.d_ = impl;

    return new_sheet;
}

worksheet
workbook::copy_sheet(worksheet to_copy, std::size_t index)
{
    copy_sheet(to_copy);

    if (index != d_->worksheets_.size() - 1) {
        auto iter = d_->worksheets_.begin();

        for (std::size_t i = 0; i < index; ++i, ++iter) { }

        d_->worksheets_.insert(iter, d_->worksheets_.back());
        d_->worksheets_.pop_back();
    }

    return sheet_by_index(index);
}

std::size_t
workbook::index(worksheet ws) const
{
    auto match = std::find(begin(), end(), ws);

    if (match == end()) {
        throw invalid_parameter("worksheet not found in workbook");
    }

    return static_cast<std::size_t>(std::distance(begin(), match));
}

void
workbook::move_sheet(worksheet worksheet, std::size_t newIndex)
{
    if (newIndex >= sheet_count()) {
        throw invalid_parameter("invalid new worksheet index " + std::to_string(newIndex)
            + " (workbook contains " + std::to_string(sheet_count()) + " worksheets)");
    }

    auto sourcePosition = d_->worksheets_.end();
    auto targetPosition = d_->worksheets_.begin();
    size_t currentIndex = 0;
    size_t sourceIndex = sheet_count();

    for (auto iter = d_->worksheets_.begin(); iter != d_->worksheets_.end(); ++iter) {
        if (worksheet.d_ == (&*iter)) {
            sourcePosition = iter;
            sourceIndex = currentIndex;
        }

        if (currentIndex == newIndex) {
            targetPosition = iter;
        }

        ++currentIndex;
    }

    if (sourcePosition == d_->worksheets_.end()) {
        throw invalid_parameter("cannot move worksheet: it does not exist in this workbook");
    }

    if (sourceIndex < newIndex) {
        ++targetPosition;
    }

    d_->worksheets_.splice(targetPosition, d_->worksheets_, sourcePosition);
}

void
workbook::create_named_range(
    const std::string& name, worksheet range_owner, const std::string& reference_string)
{
    create_named_range(name, range_owner, range_reference(reference_string));
}

void
workbook::create_named_range(
    const std::string& name, worksheet range_owner, const range_reference& reference)
{
    sheet_by_title(range_owner.title()).create_named_range(name, reference);
}

void
workbook::remove_named_range(const std::string& name)
{
    for (auto ws : *this) {
        if (ws.has_named_range(name)) {
            ws.remove_named_range(name);
            return;
        }
    }

    throw key_not_found(name);
}

range
workbook::named_range(const std::string& name)
{
    for (auto ws : *this) {
        if (ws.has_named_range(name)) {
            return ws.named_range(name);
        }
    }

    throw key_not_found(name);
}

void
workbook::load(std::istream& stream)
{
    clear();
    detail::xlsx_consumer consumer(*this);

    try {
        consumer.read(stream);
    } catch (const xlnt::invalid_password&) {
        stream.seekg(0, std::ios::beg);
        consumer.read(stream, "VelvetSweatshop");
    }
}

void
workbook::load(const std::vector<std::uint8_t>& data)
{
    if (data.size() < 22) // the shortest ZIP file is 22 bytes
    {
        throw xlnt::invalid_file(
            "file is empty or malformed (data size " + std::to_string(data.size()) + " bytes)");
    }

    xlnt::detail::vector_istreambuf data_buffer(data);
    std::istream data_stream(&data_buffer);
    load(data_stream);
}

template <typename T>
void
workbook::load_internal(const T& filename)
{
    return load(path(filename));
}

template <typename T>
void
workbook::load_internal(const T& filename, const T& password)
{
    return load(path(filename), password);
}

void
workbook::load(const std::string& filename)
{
    return load_internal(filename);
}

void
workbook::load(const path& filename)
{
    std::ifstream file_stream;
    open_stream(file_stream, filename.string());

    if (!file_stream.good()) {
        throw xlnt::invalid_file(filename.string());
    }

    load(file_stream);
}

void
workbook::load(const std::string& filename, const std::string& password)
{
    return load_internal(filename, password);
}

template <typename T>
void
workbook::load_internal(const xlnt::path& filename, const T& password)
{
    std::ifstream file_stream;
    open_stream(file_stream, filename.string());

    if (!file_stream.good()) {
        throw xlnt::invalid_file(filename.string());
    }

    return load(file_stream, password);
}

void
workbook::load(const path& filename, const std::string& password)
{
    load_internal(filename, password);
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
void
workbook::load(const xlnt::path& filename, std::u8string_view password)
{
    load_internal(filename, password);
}
#endif

template <typename T>
void
workbook::load_internal(const std::vector<std::uint8_t>& data, const T& password)
{
    if (data.size() < 22) // the shortest ZIP file is 22 bytes
    {
        throw xlnt::invalid_file(
            "file is empty or malformed (data size " + std::to_string(data.size()) + " bytes)");
    }

    xlnt::detail::vector_istreambuf data_buffer(data);
    std::istream data_stream(&data_buffer);
    load(data_stream, password);
}

void
workbook::load(const std::vector<std::uint8_t>& data, const std::string& password)
{
    load_internal(data, password);
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
void
workbook::load(const std::vector<std::uint8_t>& data, std::u8string_view password)
{
    load_internal(data, password);
}
#endif

template <typename T>
void
workbook::load_internal(std::istream& stream, const T& password)
{
    clear();
    detail::xlsx_consumer consumer(*this);
    consumer.read(stream, password);
}

void
workbook::load(std::istream& stream, const std::string& password)
{
    load_internal(stream, password);
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
void
workbook::load(std::istream& stream, std::u8string_view password)
{
    load_internal(stream, password);
}
#endif

void
workbook::save(std::vector<std::uint8_t>& data) const
{
    xlnt::detail::vector_ostreambuf data_buffer(data);
    std::ostream data_stream(&data_buffer);
    save(data_stream);
}

template <typename T>
void
workbook::save_internal(std::vector<std::uint8_t>& data, const T& password) const
{
    xlnt::detail::vector_ostreambuf data_buffer(data);
    std::ostream data_stream(&data_buffer);
    save(data_stream, password);
}

void
workbook::save(std::vector<std::uint8_t>& data, const std::string& password) const
{
    save_internal(data, password);
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
void
workbook::save(std::vector<std::uint8_t>& data, std::u8string_view password) const
{
    save_internal(data, password);
}
#endif

template <typename T>
void
workbook::save_internal(const T& filename) const
{
    save(path(filename));
}

template <typename T>
void
workbook::save_internal(const T& filename, const T& password) const
{
    save(path(filename), password);
}

void
workbook::save(const std::string& filename) const
{
    save_internal(filename);
}

void
workbook::save(const std::string& filename, const std::string& password) const
{
    save_internal(filename, password);
}

void
workbook::save(const path& filename) const
{
    std::ofstream file_stream;
    open_stream(file_stream, filename.string());
    save(file_stream);
}

template <typename T>
void
workbook::save_internal(const xlnt::path& filename, const T& password) const
{
    std::ofstream file_stream;
    open_stream(file_stream, filename.string());
    save(file_stream, password);
}

void
workbook::save(const path& filename, const std::string& password) const
{
    save_internal(filename, password);
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
void
workbook::save(const xlnt::path& filename, std::u8string_view password) const
{
    save_internal(filename, password);
}
#endif

void
workbook::save(std::ostream& stream) const
{
    detail::xlsx_producer producer(*this);
    producer.write(stream);
}

template <typename T>
void
workbook::save_internal(std::ostream& stream, const T& password) const
{
    detail::xlsx_producer producer(*this);
    producer.write(stream, password);
}

void
workbook::save(std::ostream& stream, const std::string& password) const
{
    save_internal(stream, password);
}

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
void
workbook::save(std::ostream& stream, std::u8string_view password) const
{
    save_internal(stream, password);
}
#endif

#ifdef _MSC_VER
void
workbook::save(const std::wstring& filename) const
{
    std::ofstream file_stream;
    open_stream(file_stream, filename);
    save(file_stream);
}

void
workbook::save(const std::wstring& filename, const std::string& password) const
{
    std::ofstream file_stream;
    open_stream(file_stream, filename);
    save(file_stream, password);
}

void
workbook::load(const std::wstring& filename)
{
    std::ifstream file_stream;
    open_stream(file_stream, filename);
    load(file_stream);
}

void
workbook::load(const std::wstring& filename, const std::string& password)
{
    std::ifstream file_stream;
    open_stream(file_stream, filename);
    load(file_stream, password);
}
#endif

#if XLNT_HAS_FEATURE(U8_STRING_VIEW)
void
workbook::save(std::u8string_view filename) const
{
    save_internal(filename);
}

void
workbook::save(std::u8string_view filename, std::u8string_view password) const
{
    save_internal(filename, password);
}

void
workbook::load(std::u8string_view filename)
{
    load_internal(filename);
}

void
workbook::load(std::u8string_view filename, std::u8string_view password)
{
    load_internal(filename, password);
}
#endif

void
workbook::remove_sheet(worksheet ws)
{
    auto match_iter = std::find_if(d_->worksheets_.begin(), d_->worksheets_.end(),
        [=](detail::worksheet_impl& comp) { return &comp == ws.d_; });

    if (match_iter == d_->worksheets_.end()) {
        throw invalid_parameter("cannot remove worksheet: it does not exist in this workbook");
    }

    auto ws_rel_id = d_->sheet_title_rel_id_map_.at(ws.title());
    auto wb_rel = d_->manifest_.relationship(path("/"), xlnt::relationship_type::office_document);
    auto ws_rel = d_->manifest_.relationship(wb_rel.target().path(), ws_rel_id);
    auto ws_part = d_->manifest_.canonicalize({ wb_rel, ws_rel }).resolve(path("/"));
    d_->manifest_.unregister_override_type(ws_part);
    auto rel_id_map = d_->manifest_.unregister_relationship(wb_rel.target(), ws_rel_id);
    d_->sheet_title_rel_id_map_.erase(ws.title());
    d_->worksheets_.erase(match_iter);

    // Shift sheet title->ID mappings down as a result of manifest::unregister_relationship above.
    for (auto& title_rel_id_pair : d_->sheet_title_rel_id_map_) {
        auto rel_id = rel_id_map.find(title_rel_id_pair.second);

        if (rel_id != rel_id_map.end()) {
            title_rel_id_pair.second = rel_id->second;
        }
    }

    update_sheet_properties();
}

worksheet
workbook::create_sheet(std::size_t index)
{
    create_sheet();

    if (index != d_->worksheets_.size() - 1) {
        auto iter = d_->worksheets_.begin();

        for (std::size_t i = 0; i < index; ++i, ++iter) { }

        d_->worksheets_.insert(iter, d_->worksheets_.back());
        d_->worksheets_.pop_back();
    }

    return sheet_by_index(index);
}

worksheet
workbook::create_sheet_with_rel(const std::string& title, const relationship& rel)
{
    auto sheet_id = d_->worksheets_.size() + 1;
    d_->worksheets_.push_back(detail::worksheet_impl(this, sheet_id, title));

    auto workbook_rel = d_->manifest_.relationship(path("/"), relationship_type::office_document);
    auto sheet_absoulute_path = workbook_rel.target().path().parent().append(rel.target().path());
    d_->manifest_.register_override_type(sheet_absoulute_path,
        "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml");
    auto ws_rel = d_->manifest_.register_relationship(
        workbook_rel.target(), relationship_type::worksheet, rel.target(), target_mode::internal);
    d_->sheet_title_rel_id_map_[title] = ws_rel;

    update_sheet_properties();

    return worksheet(&d_->worksheets_.back());
}

workbook::iterator
workbook::begin()
{
    return iterator(*this, 0);
}

workbook::iterator
workbook::end()
{
    return iterator(*this, d_->worksheets_.size());
}

workbook::const_iterator
workbook::begin() const
{
    return cbegin();
}

workbook::const_iterator
workbook::end() const
{
    return cend();
}

workbook::const_iterator
workbook::cbegin() const
{
    return const_iterator(*this, 0);
}

workbook::const_iterator
workbook::cend() const
{
    return const_iterator(*this, d_->worksheets_.size());
}

std::vector<std::string>
workbook::sheet_titles() const
{
    std::vector<std::string> names;

    for (auto ws : *this) {
        names.push_back(ws.title());
    }

    return names;
}

std::size_t
workbook::sheet_count() const
{
    return d_->worksheets_.size();
}

worksheet
workbook::operator[](const std::string& name)
{
    return sheet_by_title(name);
}

worksheet
workbook::operator[](std::size_t index)
{
    return sheet_by_index(index);
}

void
workbook::clear()
{
    *d_ = detail::workbook_impl();
    d_->stylesheet_.clear();
}

bool
workbook::compare(const workbook& other, bool compare_by_reference) const
{
    if (compare_by_reference) {
        return d_ == other.d_;
    } else {
        return *d_ == *other.d_;
    }
}

bool
workbook::operator==(const workbook& rhs) const
{
    return compare(rhs, true);
}

bool
workbook::operator!=(const workbook& rhs) const
{
    return !operator==(rhs);
}

void
workbook::swap(workbook& right)
{
    std::swap(d_, right.d_);
}

workbook
workbook::clone(clone_method method) const
{
    switch (method) {
    case clone_method::deep_copy: {
        workbook wb;
        *wb.d_ = *d_;

        for (auto ws : wb) {
            ws.parent(wb);
        }

        wb.d_->stylesheet_.get().parent = wb.d_;

        return wb;
    }
    case clone_method::shallow_copy: {
        return workbook(d_);
    }
    default:
        throw xlnt::invalid_parameter(
            "clone method " + std::to_string(static_cast<int>(method)) + " not supported");
    }
}

bool
workbook::has_theme() const
{
    return d_->theme_.is_set();
}

const theme&
workbook::theme() const
{
    if (!d_->theme_.is_set()) {
        throw xlnt::invalid_attribute("the workbook has no theme");
    }
    return d_->theme_.get();
}

void
workbook::theme(const class theme& value)
{
    d_->theme_ = value;
    register_workbook_part(relationship_type::theme);
}

std::vector<named_range>
workbook::named_ranges() const
{
    std::vector<xlnt::named_range> named_ranges;

    for (auto ws : *this) {
        for (auto& ws_named_range : ws.d_->named_ranges_) {
            named_ranges.push_back(ws_named_range.second);
        }
    }

    return named_ranges;
}

format
workbook::create_format(bool default_format)
{
    register_workbook_part(relationship_type::stylesheet);
    return d_->stylesheet_.get().create_format(default_format);
}

format
workbook::clone_format_from(const xlnt::format& source_format)
{
    // Start with a blank format from this workbook (not default format)
    auto cloned_format = create_format();

    // Copy all format properties using public API
    if (source_format.alignment_applied()) {
        cloned_format.alignment(source_format.alignment(), true);
    }

    if (source_format.border_applied()) {
        cloned_format.border(source_format.border(), true);
    }

    if (source_format.fill_applied()) {
        cloned_format.fill(source_format.fill(), true);
    }

    if (source_format.font_applied()) {
        cloned_format.font(source_format.font(), true);
    }

    if (source_format.number_format_applied()) {
        cloned_format.number_format(source_format.number_format(), true);
    }

    if (source_format.protection_applied()) {
        cloned_format.protection(source_format.protection(), true);
    }

    // Copy other properties (no "applied" flag for these)
    cloned_format.pivot_button(source_format.pivot_button());
    cloned_format.quote_prefix(source_format.quote_prefix());

    if (source_format.has_style()) {
        // Style assignment is automatically deduplicating:
        // xlnt reuses existing style objects if identical, or creates new ones if needed.
        // This prevents duplicate styles in the destination workbook.
        // This function is only copying the name using the format pattern from "cloned_format".
        cloned_format.style(source_format.style());
    }

    return cloned_format;
}

bool
workbook::owns_format(const class format& fmt) const
{
    if (!d_->stylesheet_.is_set()) {
        return false;
    }

    return fmt.d_->parent == &d_->stylesheet_.get();
}

void
workbook::default_format(const class format& format)
{
    d_->stylesheet_.get().default_format(format);
}

bool
workbook::has_style(const std::string& name) const
{
    return d_->stylesheet_.get().has_style(name);
}

void
workbook::clear_styles()
{
    apply_to_cells([](cell c) { c.clear_style(); });
}

void
workbook::default_slicer_style(const std::string& value)
{
    d_->stylesheet_.get().default_slicer_style = value;
}

std::string
workbook::default_slicer_style() const
{
    return d_->stylesheet_.get().default_slicer_style.get();
}

void
workbook::enable_known_fonts()
{
    d_->stylesheet_.get().known_fonts_enabled = true;
}

void
workbook::disable_known_fonts()
{
    d_->stylesheet_.get().known_fonts_enabled = false;
}

bool
workbook::known_fonts_enabled() const
{
    return d_->stylesheet_.get().known_fonts_enabled;
}

void
workbook::clear_formats()
{
    apply_to_cells([](cell c) { c.clear_format(); });
}

std::size_t
workbook::format_count() const
{
    return d_->stylesheet_.get().format_impls.size();
}

void
workbook::apply_to_cells(std::function<void(cell)> f)
{
    for (auto ws : *this) {
        for (auto row = ws.lowest_row(); row <= ws.highest_row(); ++row) {
            for (auto column = ws.lowest_column(); column <= ws.highest_column(); ++column) {
                if (ws.has_cell(cell_reference(column, row))) {
                    f.operator()(ws.cell(cell_reference(column, row)));
                }
            }
        }
    }
}

format
workbook::format(std::size_t format_index)
{
    return d_->stylesheet_.get().format(format_index);
}

const format
workbook::format(std::size_t format_index) const
{
    return d_->stylesheet_.get().format(format_index);
}

manifest&
workbook::manifest()
{
    return d_->manifest_;
}

const manifest&
workbook::manifest() const
{
    return d_->manifest_;
}

const rich_text&
workbook::shared_strings(std::size_t index) const
{
    if (index < d_->shared_strings_values_.size()) {
        return d_->shared_strings_values_.at(index);
    }

    static rich_text empty;
    return empty;
}

std::vector<rich_text>&
workbook::shared_strings()
{
    return d_->shared_strings_values_;
}

const std::vector<rich_text>&
workbook::shared_strings() const
{
    return d_->shared_strings_values_;
}

std::size_t
workbook::add_shared_string(const rich_text& shared, bool allow_duplicates)
{
    register_workbook_part(relationship_type::shared_string_table);

    if (!allow_duplicates) {
        auto it = d_->shared_strings_ids_.find(shared);

        if (it != d_->shared_strings_ids_.end()) {
            return it->second;
        }
    }

    auto sz = d_->shared_strings_ids_.size();
    d_->shared_strings_ids_[shared] = sz;
    d_->shared_strings_values_.push_back(shared);

    return sz;
}

bool
workbook::contains(const std::string& sheet_title) const
{
    for (auto ws : *this) {
        if (ws.title() == sheet_title) {
            return true;
        }
    }

    return false;
}

void
workbook::thumbnail(const std::vector<std::uint8_t>& thumbnail, const std::string& extension,
    const std::string& content_type)
{
    if (!d_->manifest_.has_relationship(path("/"), relationship_type::thumbnail)) {
        d_->manifest_.register_default_type(extension, content_type);
        d_->manifest_.register_relationship(uri("/"), relationship_type::thumbnail,
            uri("docProps/thumbnail.jpeg"), target_mode::internal);
    }

    auto thumbnail_rel = d_->manifest_.relationship(path("/"), relationship_type::thumbnail);
    d_->images_[thumbnail_rel.target().to_string()] = thumbnail;
}

bool
workbook::has_thumbnail() const
{
    auto thumbnail_rel = d_->manifest_.relationship(path("/"), relationship_type::thumbnail);
    return d_->images_.count(thumbnail_rel.target().to_string()) > 0;
}

const std::vector<std::uint8_t>&
workbook::thumbnail() const
{
    auto thumbnail_rel = d_->manifest_.relationship(path("/"), relationship_type::thumbnail);
    auto thumbnail = d_->images_.find(thumbnail_rel.target().to_string());

    if (thumbnail == d_->images_.end()) {
        throw xlnt::invalid_attribute("the workbook has no thumbnail");
    }

    return thumbnail->second;
}

const std::unordered_map<std::string, std::vector<std::uint8_t>>&
workbook::binaries() const
{
    return d_->binaries_;
}

style
workbook::create_style(const std::string& name)
{
    return d_->stylesheet_.get().create_style(name);
}

style
workbook::create_builtin_style(const std::size_t builtin_id)
{
    return d_->stylesheet_.get().create_builtin_style(builtin_id);
}

style
workbook::style(const std::string& name)
{
    return d_->stylesheet_.get().style(name);
}

const style
workbook::style(const std::string& name) const
{
    return d_->stylesheet_.get().style(name);
}

calendar
workbook::base_date() const
{
    return d_->base_date_;
}

void
workbook::base_date(calendar base_date)
{
    d_->base_date_ = base_date;
}

bool
workbook::has_title() const
{
    return d_->title_.is_set();
}

std::string
workbook::title() const
{
    if (!d_->title_.is_set()) {
        throw xlnt::invalid_attribute("the workbook has no title");
    }
    return d_->title_.get();
}

void
workbook::title(const std::string& title)
{
    d_->title_ = title;
}

detail::workbook_impl&
workbook::impl()
{
    return *d_;
}

const detail::workbook_impl&
workbook::impl() const
{
    return *d_;
}

bool
workbook::has_view() const
{
    return d_->view_.is_set();
}

workbook_view
workbook::view() const
{
    if (!d_->view_.is_set()) {
        throw invalid_attribute("the workbook has no view");
    }

    return d_->view_.get();
}

void
workbook::view(const workbook_view& view)
{
    d_->view_ = view;
}

bool
workbook::has_code_name() const
{
    return d_->code_name_.is_set();
}

std::string
workbook::code_name() const
{
    if (!has_code_name()) {
        throw invalid_attribute("the workbook has no code name");
    }

    return d_->code_name_.get();
}

void
workbook::code_name(const std::string& code_name)
{
    d_->code_name_ = code_name;
}

bool
workbook::has_file_version() const
{
    return d_->file_version_.is_set();
}

void
workbook::clear_file_version()
{
    d_->file_version_.clear();
}

bool
workbook::has_app_name() const
{
    return d_->file_version_.is_set() && !d_->file_version_.get().app_name.empty();
}

const std::string&
workbook::app_name() const
{
    if (!d_->file_version_.is_set()) {
        return constants::empty_str();
    }
    return d_->file_version_.get().app_name;
}

void
workbook::app_name(const std::string& app_name)
{
    if (!d_->file_version_.is_set()) {
        d_->file_version_ = detail::workbook_impl::file_version_t();
    }

    d_->file_version_.get().app_name = app_name;
}

bool
workbook::has_last_edited() const
{
    return d_->file_version_.is_set() && !d_->file_version_.get().last_edited.empty();
}

const std::string&
workbook::last_edited_str() const
{
    if (!d_->file_version_.is_set()) {
        return constants::empty_str();
    }
    return d_->file_version_.get().last_edited;
}

std::size_t
workbook::last_edited() const
{
    if (!has_last_edited()) {
        throw xlnt::invalid_attribute("the workbook has no last edited property");
    }

    size_t last_edited = 0;
    size_t num_parsed_chars = 0;
    if (detail::parse(d_->file_version_.get().last_edited, last_edited, &num_parsed_chars)
            != std::errc()
        || num_parsed_chars != d_->file_version_.get().last_edited.length()) {
        throw xlnt::invalid_attribute("the last edited property could not be parsed from string \""
            + d_->file_version_.get().last_edited + "\"");
    }
    return last_edited;
}

void
workbook::last_edited(const std::string& last_edited)
{
    if (!d_->file_version_.is_set()) {
        d_->file_version_ = detail::workbook_impl::file_version_t();
    }

    d_->file_version_.get().last_edited = last_edited;
}

void
workbook::last_edited(std::size_t last_edited)
{
    if (!d_->file_version_.is_set()) {
        d_->file_version_ = detail::workbook_impl::file_version_t();
    }

    d_->file_version_.get().last_edited = std::to_string(last_edited);
}

bool
workbook::has_lowest_edited() const
{
    return d_->file_version_.is_set() && !d_->file_version_.get().lowest_edited.empty();
}

const std::string&
workbook::lowest_edited_str() const
{
    if (!d_->file_version_.is_set()) {
        return constants::empty_str();
    }
    return d_->file_version_.get().lowest_edited;
}

std::size_t
workbook::lowest_edited() const
{
    if (!has_lowest_edited()) {
        throw xlnt::invalid_attribute("the workbook has no lowest edited property");
    }

    size_t lowest_edited = 0;
    size_t num_parsed_chars = 0;
    if (detail::parse(d_->file_version_.get().lowest_edited, lowest_edited, &num_parsed_chars)
            != std::errc()
        || num_parsed_chars != d_->file_version_.get().lowest_edited.length()) {
        throw xlnt::invalid_attribute(
            "the lowest edited property could not be parsed from string \""
            + d_->file_version_.get().lowest_edited + "\"");
    }
    return lowest_edited;
}

void
workbook::lowest_edited(const std::string& lowest_edited)
{
    if (!d_->file_version_.is_set()) {
        d_->file_version_ = detail::workbook_impl::file_version_t();
    }

    d_->file_version_.get().lowest_edited = lowest_edited;
}

void
workbook::lowest_edited(std::size_t lowest_edited)
{
    if (!d_->file_version_.is_set()) {
        d_->file_version_ = detail::workbook_impl::file_version_t();
    }

    d_->file_version_.get().lowest_edited = std::to_string(lowest_edited);
}

bool
workbook::has_rup_build() const
{
    return d_->file_version_.is_set() && !d_->file_version_.get().rup_build.empty();
}

const std::string&
workbook::rup_build_str() const
{
    if (!d_->file_version_.is_set()) {
        return constants::empty_str();
    }
    return d_->file_version_.get().rup_build;
}

std::size_t
workbook::rup_build() const
{
    if (!has_rup_build()) {
        throw xlnt::invalid_attribute("the workbook has no rup_build property");
    }

    size_t rup_build = 0;
    size_t num_parsed_chars = 0;
    if (detail::parse(d_->file_version_.get().rup_build, rup_build, &num_parsed_chars)
            != std::errc()
        || num_parsed_chars != d_->file_version_.get().rup_build.length()) {
        throw xlnt::invalid_attribute("the rup_build property could not be parsed from string \""
            + d_->file_version_.get().rup_build + "\"");
    }
    return rup_build;
}

void
workbook::rup_build(const std::string& rup_build)
{
    if (!d_->file_version_.is_set()) {
        d_->file_version_ = detail::workbook_impl::file_version_t();
    }

    d_->file_version_.get().rup_build = rup_build;
}

void
workbook::rup_build(std::size_t rup_build)
{
    if (!d_->file_version_.is_set()) {
        d_->file_version_ = detail::workbook_impl::file_version_t();
    }

    d_->file_version_.get().rup_build = std::to_string(rup_build);
}

bool
workbook::has_calculation_properties() const
{
    return d_->calculation_properties_.is_set();
}

void
workbook::clear_calculation_properties()
{
    d_->calculation_properties_.clear();
}

class calculation_properties
workbook::calculation_properties() const
{
    if (!d_->calculation_properties_.is_set()) {
        return {};
    }
    return d_->calculation_properties_.get();
}

void
workbook::calculation_properties(const class calculation_properties& props)
{
    d_->calculation_properties_ = props;
}

void
workbook::garbage_collect_formulae()
{
    auto any_with_formula = false;

    for (auto ws : *this) {
        for (auto row : ws.rows(true)) {
            for (auto cell : row) {
                if (cell.has_formula()) {
                    any_with_formula = true;
                }
            }
        }
    }

    if (any_with_formula) {
        return;
    }

    auto wb_rel = manifest().relationship(path("/"), relationship_type::office_document);

    if (manifest().has_relationship(wb_rel.target().path(), relationship_type::calculation_chain)) {
        auto calc_chain_rel
            = manifest().relationship(wb_rel.target().path(), relationship_type::calculation_chain);
        auto calc_chain_part = manifest().canonicalize({ wb_rel, calc_chain_rel });
        manifest().unregister_override_type(calc_chain_part);
        manifest().unregister_relationship(wb_rel.target(), calc_chain_rel.id());
    }
}

void
workbook::update_sheet_properties()
{
    if (has_extended_property(xlnt::extended_property::titles_of_parts)) {
        extended_property(xlnt::extended_property::titles_of_parts, sheet_titles());
    }

    if (has_extended_property(xlnt::extended_property::heading_pairs)) {
        extended_property(xlnt::extended_property::heading_pairs,
            std::vector<variant> {
                variant("Worksheets"), variant(static_cast<int>(sheet_count())) });
    }
}

namespace {
    // true if a sheet index is != worksheet relationship index
    bool
    needs_reorder(const std::unordered_map<std::string, std::string>& title_to_rels,
        const std::vector<std::string>& titles, std::vector<std::string>& relation_ids)
    {
        bool all_match = true;
        for (std::size_t title_index = 0; title_index < titles.size(); ++title_index) {
            const auto& rel = title_to_rels.at(titles[title_index]);
            relation_ids.push_back(rel);
            const auto expected_rel_id = "rId" + std::to_string(title_index + 1);
            if (rel != expected_rel_id) {
                all_match = false;
            }
        }
        return !all_match; // if all are as expected, reorder not required
    }

    struct rel_id_sorter
    {
        // true if lhs < rhs
        bool
        operator()(const xlnt::relationship& lhs, const xlnt::relationship& rhs)
        {
            // format is rTd<decimal number 1..n>
            if (lhs.id().size() != rhs.id().size()) // a number with more digits will be larger
            {
                return lhs.id().size() < rhs.id().size();
            }
            return lhs.id() < rhs.id();
        }
    };
} // namespace

void
workbook::reorder_relationships()
{
    const auto titles = sheet_titles();
    // the relation ID corresponding to the title at the same index is copied into here
    std::vector<std::string> worksheet_rel_ids;
    worksheet_rel_ids.reserve(titles.size());
    if (!needs_reorder(d_->sheet_title_rel_id_map_, titles, worksheet_rel_ids)) {
        return;
    }
    // copy of existing relations
    const auto wb_rel_target
        = manifest().relationship(path("/"), relationship_type::office_document).target();
    auto rel_copy = manifest().relationships(wb_rel_target.path());
    std::sort(rel_copy.begin(), rel_copy.end(), rel_id_sorter {});
    // clear existing relations
    for (const auto& rel : rel_copy) {
        manifest().unregister_relationship(wb_rel_target, rel.id());
    }
    // create new relations
    std::size_t index = 0;
    auto new_id = [&index]() { return "rId" + std::to_string(++index); }; // ids start from 1
    // worksheets first
    while (index < worksheet_rel_ids.size()) {
        auto rel_it = std::find_if(rel_copy.begin(), rel_copy.end(),
            [&](const relationship& rel) { return rel.id() == worksheet_rel_ids[index]; });

        if (rel_it == rel_copy.end()) {
            throw xlnt::key_not_found(worksheet_rel_ids[index]);
        }

        std::string rel_id = new_id();
        d_->sheet_title_rel_id_map_.at(titles[index - 1])
            = rel_id; // update title -> relation mapping
        manifest().register_relationship(relationship(
            rel_id, rel_it->type(), rel_it->source(), rel_it->target(), rel_it->target_mode()));
    }
    // then all the other relations in the same order they started (just new indices)
    for (const auto& old_rel : rel_copy) {
        if (old_rel.type() == relationship_type::worksheet) {
            continue;
        }
        manifest().register_relationship(relationship(
            new_id(), old_rel.type(), old_rel.source(), old_rel.target(), old_rel.target_mode()));
    }
}

} // namespace xlnt

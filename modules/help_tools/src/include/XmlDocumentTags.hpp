//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================

namespace Nelson {
#define XML_FILE_EXTENSION ".xml"
#define XML_ATTRIB_LINKEND_TAG "<xmlattr>.linkend"
#define XML_ATTRIB_TAG "<xmlattrib>"
#define XML_COMMENT_TAG "<xmlcomment>"
#define NELSON_EXAMPLE_TYPE "nelson"

#define XML_LINKEND_TAG "linkend"
#define XML_LINK_TAG "link"
#define XMLDOC_TAG "xmldoc"
#define LANGUAGE_TAG "language"
#define TITLE_TAG "title"
#define KEYWORD_TAG "keyword"
#define CHAPTER_TAG "chapter"
#define SHORT_DESCRIPTION_TAG "short_description"
#define COPYRIGHT_TAG "copyright"
#define SYNTAX_TAG "syntax"
#define SYNTAX_ITEM_TAG "syntax_item"
#define PARAM_INPUT_TAG "param_input"
#define PARAM_INPUT_ITEM_TAG "param_input_item"
#define PARAM_NAME_TAG "param_name"
#define PARAM_DESCRIPTION_TAG "param_description"
#define PARAM_OUTPUT_TAG "param_output"
#define PARAM_OUTPUT_ITEM_TAG "param_output_item"
#define DESCRIPTION_TAG "description"
#define USED_FUNCTION_TAG "used_function"
#define BIBLIOGRAPHY_TAG "bibliography"
#define EXAMPLES_TAG "examples"
#define EXAMPLE_ITEM_TAG "example_item"
#define EXAMPLE_ITEM_TYPE_TAG "example_item_type"
#define EXAMPLE_ITEM_DESCRIPTION_TAG "example_item_description"
#define EXAMPLE_ITEM_DATA_TAG "example_item_data"
#define EXAMPLE_ITEM_IMG_TAG "example_item_img"
#define SEE_ALSO_TAG "see_also"
#define SEE_ALSO_ITEM_TAG "see_also_item"
#define HISTORY_TAG "history"
#define HISTORY_ITEM_TAG "history_item"
#define HISTORY_ITEM_VERSION_TAG "history_version"
#define HISTORY_ITEM_DESCRIPTION_TAG "history_description"
#define AUTHORS_TAG "authors"
#define AUTHOR_ITEM_TAG "author_item"
#define CHAPTER_DESCRIPTION_TAG "chapter_description"
#define CHAPTER_TAG "chapter"
#define CHAPTER_INDEX_TAG "chapter_index"
#define CHAPTER_REF_TAG "chapter_ref"
#define CHAPTER_REF_DESCRIPTION_TAG "chapter_ref_description"
#define MODULE_NAME_TAG "module_name"
#define IMAGE_TAG "img"

enum XMLDOC_ITEMS_ID
{
    ERROR_ID = -1,
    XML_COMMENT_TAG_ID = 0,
    XMLDOC_TAG_ID,
    LANGUAGE_TAG_ID,
    TITLE_TAG_ID,
    KEYWORD_TAG_ID,
    SHORT_DESCRIPTION_TAG_ID,
    COPYRIGHT_TAG_ID,
    SYNTAX_TAG_ID,
    PARAM_INPUT_TAG_ID,
    PARAM_OUTPUT_TAG_ID,
    DESCRIPTION_TAG_ID,
    USED_FUNCTION_TAG_ID,
    BIBLIOGRAPHY_TAG_ID,
    EXAMPLES_TAG_ID,
    SEE_ALSO_TAG_ID,
    HISTORY_TAG_ID,
    AUTHORS_TAG_ID,
    CHAPTER_TAG_ID,
    CHAPTER_DESCRIPTION_TAG_ID,
    CHAPTER_INDEX_TAG_ID,
    MODULE_NAME_TAG_ID,
    IMAGE_TAG_ID
};

} // namespace Nelson
//=============================================================================

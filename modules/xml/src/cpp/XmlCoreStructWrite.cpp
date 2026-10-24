//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "XmlCorePrivate.hpp"
#include <cmath>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    std::string
    arrayToText(const ArrayOf& value)
    {
        if (value.isCharacterArray() || value.isScalarStringArray()) {
            return wstring_to_utf8(value.asWideString());
        }
        if (value.isLogical() && value.isScalar()) {
            return value.asLogicalScalar() ? "true" : "false";
        }
        if (value.isNumeric() && value.isScalar()) {
            double numericValue = value.asDoubleScalar();
            if (std::isnan(numericValue)) {
                return "";
            }
            if (std::isinf(numericValue)) {
                return numericValue > 0 ? "Inf" : "-Inf";
            }
            std::ostringstream stream;
            stream.precision(15);
            stream << numericValue;
            return stream.str();
        }
        return "";
    }
    //=============================================================================
    bool
    endsWith(const std::string& value, const std::string& suffix)
    {
        return value.size() >= suffix.size()
            && value.compare(value.size() - suffix.size(), suffix.size(), suffix) == 0;
    }
    //=============================================================================
    void
    appendValueToNode(xmlNodePtr node, const ArrayOf& value, const XmlStructWriteOptions& options);
    void
    appendStructFields(xmlNodePtr node, const ArrayOf& value, const XmlStructWriteOptions& options);
    //=============================================================================
    ArrayOf
    structElementAt(const ArrayOf& value, indexType index)
    {
        stringVector fields = value.fieldNames();
        ArrayOfVector fieldValues;
        for (const std::string& name : fields) {
            ArrayOfVector values = value.fieldAsList(name);
            fieldValues << values[index];
        }
        return ArrayOf::structScalar(fields, fieldValues);
    }
    //=============================================================================
    bool
    appendStructArrayField(xmlNodePtr node, const std::string& field, const ArrayOf& value,
        const XmlStructWriteOptions& options)
    {
        if (!value.isStruct() || value.elementCount() <= 1) {
            return false;
        }
        for (indexType k = 0; k < value.elementCount(); k++) {
            xmlNodePtr child = xmlNewChild(node, nullptr, BAD_CAST field.c_str(), nullptr);
            appendStructFields(child, structElementAt(value, k), options);
        }
        return true;
    }
    //=============================================================================
    bool
    appendArrayField(xmlNodePtr node, const std::string& field, const ArrayOf& value)
    {
        if (value.isStringArray() && !value.isScalar()) {
            auto* elements = static_cast<const ArrayOf*>(value.dataPointer());
            for (indexType k = 0; k < value.elementCount(); k++) {
                xmlNodePtr child = xmlNewChild(node, nullptr, BAD_CAST field.c_str(), nullptr);
                xmlNodeSetContent(child, BAD_CAST arrayToText(elements[k]).c_str());
            }
            return true;
        }
        if (value.isNumeric() && !value.isScalar() && !value.isComplex()) {
            std::vector<double> values = value.asDoubleVector();
            for (double item : values) {
                xmlNodePtr child = xmlNewChild(node, nullptr, BAD_CAST field.c_str(), nullptr);
                xmlNodeSetContent(child, BAD_CAST arrayToText(ArrayOf::doubleScalar(item)).c_str());
            }
            return true;
        }
        if (value.isLogical() && !value.isScalar()) {
            auto* values = static_cast<const logical*>(value.dataPointer());
            for (indexType k = 0; k < value.elementCount(); k++) {
                xmlNodePtr child = xmlNewChild(node, nullptr, BAD_CAST field.c_str(), nullptr);
                xmlNodeSetContent(
                    child, BAD_CAST arrayToText(ArrayOf::logicalScalar(values[k] != 0)).c_str());
            }
            return true;
        }
        return false;
    }
    //=============================================================================
    void
    appendStructFields(xmlNodePtr node, const ArrayOf& value, const XmlStructWriteOptions& options)
    {
        stringVector fieldNames = value.fieldNames();
        for (const std::string& field : fieldNames) {
            ArrayOfVector items = value.fieldAsList(field);
            if (field == "Text" && !items.empty() && !items.front().isMissing()) {
                xmlNodeSetContent(node, BAD_CAST arrayToText(items.front()).c_str());
                continue;
            }
            if (endsWith(field, options.attributeSuffix) && items.size() == 1
                && !items.front().isMissing()) {
                std::string attr = field.substr(0, field.size() - options.attributeSuffix.size());
                xmlSetProp(
                    node, BAD_CAST attr.c_str(), BAD_CAST arrayToText(items.front()).c_str());
                continue;
            }
            if (items.size() == 1 && appendStructArrayField(node, field, items.front(), options)) {
                continue;
            }
            if (items.size() == 1 && appendArrayField(node, field, items.front())) {
                continue;
            }
            for (const ArrayOf& item : items) {
                if (item.isMissing()) {
                    continue;
                }
                xmlNodePtr child = xmlNewChild(node, nullptr, BAD_CAST field.c_str(), nullptr);
                appendValueToNode(child, item, options);
            }
        }
    }
    //=============================================================================
    void
    appendCellItems(xmlNodePtr node, const ArrayOf& value, const XmlStructWriteOptions& options)
    {
        auto* items = static_cast<const ArrayOf*>(value.dataPointer());
        for (indexType k = 0; k < value.elementCount(); k++) {
            xmlNodePtr child = xmlNewChild(node, nullptr, BAD_CAST "item", nullptr);
            appendValueToNode(child, items[k], options);
        }
    }
    //=============================================================================
    void
    appendValueToNode(xmlNodePtr node, const ArrayOf& value, const XmlStructWriteOptions& options)
    {
        if ((value.isStruct() || value.isClassType()) && value.elementCount() > 0) {
            appendStructFields(node, value, options);
            return;
        }
        if (value.isCell()) {
            appendCellItems(node, value, options);
            return;
        }
        xmlNodeSetContent(node, BAD_CAST arrayToText(value).c_str());
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
XmlWriteStruct(const ArrayOf& value, const std::wstring& filename,
    const XmlStructWriteOptions& options, std::wstring& errorMessage)
{
    if (!options.fileType.empty() && options.fileType != "xml") {
        errorMessage = L"Only XML FileType is supported by writestruct.";
        return false;
    }
    if (!value.isStruct() && !value.isClassType()) {
        errorMessage = L"Structure expected.";
        return false;
    }
    xmlDocPtr doc = xmlNewDoc(BAD_CAST "1.0");
    xmlNodePtr root = xmlNewNode(nullptr, BAD_CAST options.structNodeName.c_str());
    xmlDocSetRootElement(doc, root);
    appendStructFields(root, value, options);
    std::string output;
    bool ok = XmlCorePrivate::serializeDoc(doc, options.prettyPrint, output, errorMessage);
    xmlFreeDoc(doc);
    if (!ok) {
        return false;
    }
    if (!XmlCorePrivate::writeFileUtf8(filename, output)) {
        errorMessage = L"Unable to write XML file.";
        return false;
    }
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

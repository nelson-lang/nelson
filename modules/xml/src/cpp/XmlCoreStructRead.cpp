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
#include "MakeValidFieldname.hpp"
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <limits>
#include <map>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    std::string
    xmlName(xmlNodePtr node)
    {
        if (node == nullptr || node->name == nullptr) {
            return "node";
        }
        std::string name = reinterpret_cast<const char*>(node->name);
        if (node->ns != nullptr && node->ns->prefix != nullptr) {
            return std::string(reinterpret_cast<const char*>(node->ns->prefix)) + "_" + name;
        }
        return name;
    }
    //=============================================================================
    std::string
    xmlAttributeName(xmlAttrPtr attr)
    {
        if (attr == nullptr || attr->name == nullptr) {
            return "attribute";
        }
        std::string name = reinterpret_cast<const char*>(attr->name);
        if (attr->ns != nullptr && attr->ns->prefix != nullptr) {
            return std::string(reinterpret_cast<const char*>(attr->ns->prefix)) + "_" + name;
        }
        return name;
    }
    //=============================================================================
    std::string
    validFieldName(const std::string& name)
    {
        return MakeValidFieldname(name.empty() ? "x" : name);
    }
    //=============================================================================
    std::string
    uniqueFieldName(const stringVector& names, const std::string& candidate)
    {
        std::string name = candidate;
        size_t index = 1;
        while (std::find(names.begin(), names.end(), name) != names.end()) {
            name = candidate + "_" + std::to_string(index);
            index++;
        }
        return name;
    }
    //=============================================================================
    std::string
    directText(xmlNodePtr node)
    {
        std::string text;
        for (xmlNodePtr child = node->children; child != nullptr; child = child->next) {
            if (child->type == XML_TEXT_NODE || child->type == XML_CDATA_SECTION_NODE) {
                if (child->content != nullptr) {
                    text += reinterpret_cast<const char*>(child->content);
                }
            }
        }
        return XmlCorePrivate::trimUtf8(text);
    }
    //=============================================================================
    ArrayOf
    textToArray(const std::string& text)
    {
        std::string trimmed = XmlCorePrivate::trimUtf8(text);
        char* end = nullptr;
        double value = std::strtod(trimmed.c_str(), &end);
        if (!trimmed.empty() && end != trimmed.c_str() && *end == '\0') {
            return ArrayOf::doubleScalar(value);
        }
        return ArrayOf::stringArray(text);
    }
    //=============================================================================
    bool
    isSimpleRepeatedValue(const ArrayOf& value)
    {
        return (value.isScalar() && value.isDoubleType(true)) || value.isRowVectorCharacterArray()
            || value.isScalarStringArray();
    }
    //=============================================================================
    bool
    simpleRepeatedValues(const std::vector<ArrayOf>& values)
    {
        return std::all_of(values.begin(), values.end(), isSimpleRepeatedValue);
    }
    //=============================================================================
    double
    simpleValueToDouble(const ArrayOf& value)
    {
        if (value.isDoubleType(true)) {
            return value.asDoubleScalar();
        }
        std::string text = wstring_to_utf8(value.asWideString());
        char* end = nullptr;
        double numericValue = std::strtod(text.c_str(), &end);
        if (!text.empty() && end != text.c_str() && *end == '\0') {
            return numericValue;
        }
        return std::numeric_limits<double>::quiet_NaN();
    }
    //=============================================================================
    ArrayOf
    makeSimpleRepeatedValue(const std::vector<ArrayOf>& values)
    {
        bool hasNumeric = std::any_of(values.begin(), values.end(),
            [](const ArrayOf& value) { return value.isDoubleType(true); });
        if (hasNumeric) {
            std::vector<double> numericValues;
            numericValues.reserve(values.size());
            for (const ArrayOf& value : values) {
                numericValues.push_back(simpleValueToDouble(value));
            }
            return ArrayOf::doubleRowVector(numericValues);
        }
        stringVector stringValues;
        stringValues.reserve(values.size());
        for (const ArrayOf& value : values) {
            stringValues.push_back(wstring_to_utf8(value.asWideString()));
        }
        Dimensions dims(1, static_cast<indexType>(stringValues.size()));
        return ArrayOf::stringArray(stringValues, dims);
    }
    //=============================================================================
    ArrayOf
    makeCellRow(const std::vector<ArrayOf>& values)
    {
        Dimensions dims(1, static_cast<indexType>(values.size()));
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount(), stringVector(), false));
        for (size_t k = 0; k < values.size(); k++) {
            elements[k] = values[k];
        }
        return ArrayOf(NLS_CELL_ARRAY, dims, elements);
    }
    //=============================================================================
    bool
    sameStructFields(const std::vector<ArrayOf>& values, stringVector& fields)
    {
        if (values.empty() || !values[0].isStruct()) {
            return false;
        }
        fields = values[0].fieldNames();
        for (const ArrayOf& value : values) {
            if (!value.isStruct() || value.fieldNames() != fields || !value.isScalar()) {
                return false;
            }
        }
        return true;
    }
    //=============================================================================
    bool
    allScalarStructs(const std::vector<ArrayOf>& values)
    {
        return !values.empty()
            && std::all_of(values.begin(), values.end(),
                [](const ArrayOf& value) { return value.isStruct() && value.isScalar(); });
    }
    //=============================================================================
    stringVector
    unionStructFields(const std::vector<ArrayOf>& values)
    {
        stringVector fields;
        for (const ArrayOf& value : values) {
            for (const std::string& field : value.fieldNames()) {
                if (std::find(fields.begin(), fields.end(), field) == fields.end()) {
                    fields.push_back(field);
                }
            }
        }
        return fields;
    }
    //=============================================================================
    ArrayOf
    makeStructRow(const std::vector<ArrayOf>& values, const stringVector& fields)
    {
        Dimensions dims(1, static_cast<indexType>(values.size()));
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fields, false));
        indexType offset = 0;
        for (const ArrayOf& value : values) {
            for (const std::string& field : fields) {
                elements[offset] = value.field(field);
                offset++;
            }
        }
        return ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fields);
    }
    //=============================================================================
    ArrayOf
    makeUnionStructRow(const std::vector<ArrayOf>& values)
    {
        stringVector fields = unionStructFields(values);
        Dimensions dims(1, static_cast<indexType>(values.size()));
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_STRUCT_ARRAY, dims.elementCount(), fields, false));
        indexType offset = 0;
        for (const ArrayOf& value : values) {
            stringVector valueFields = value.fieldNames();
            for (const std::string& field : fields) {
                if (std::find(valueFields.begin(), valueFields.end(), field) == valueFields.end()) {
                    elements[offset] = ArrayOf::missingScalar();
                } else {
                    elements[offset] = value.field(field);
                }
                offset++;
            }
        }
        return ArrayOf(NLS_STRUCT_ARRAY, dims, elements, false, fields);
    }
    //=============================================================================
    ArrayOf
    makeRepeatedValue(const std::vector<ArrayOf>& values)
    {
        if (values.size() == 1) {
            return values[0];
        }
        stringVector fields;
        if (sameStructFields(values, fields)) {
            return makeStructRow(values, fields);
        }
        if (allScalarStructs(values)) {
            return makeUnionStructRow(values);
        }
        if (simpleRepeatedValues(values)) {
            return makeSimpleRepeatedValue(values);
        }
        return makeCellRow(values);
    }
    //=============================================================================
    ArrayOf
    xmlNodeToStruct(xmlNodePtr node, const XmlStructReadOptions& options)
    {
        stringVector fieldNames;
        ArrayOfVector fieldValues;
        if (options.importAttributes) {
            for (xmlNsPtr ns = node->nsDef; ns != nullptr; ns = ns->next) {
                std::string prefix
                    = ns->prefix == nullptr ? "" : reinterpret_cast<const char*>(ns->prefix);
                std::string nsName = prefix.empty() ? "xmlns" : "xmlns_" + prefix;
                std::string name = validFieldName(nsName + options.attributeSuffix);
                name = uniqueFieldName(fieldNames, name);
                fieldNames.push_back(name);
                fieldValues << ArrayOf::stringArray(
                    ns->href == nullptr ? "" : reinterpret_cast<const char*>(ns->href));
            }
            for (xmlAttrPtr attr = node->properties; attr != nullptr; attr = attr->next) {
                xmlChar* raw = xmlNodeListGetString(node->doc, attr->children, 1);
                std::string attrName = xmlAttributeName(attr);
                std::string name = validFieldName(attrName + options.attributeSuffix);
                name = uniqueFieldName(fieldNames, name);
                fieldNames.push_back(name);
                fieldValues << ArrayOf::stringArray(
                    raw == nullptr ? "" : reinterpret_cast<const char*>(raw));
                if (raw != nullptr) {
                    xmlFree(raw);
                }
            }
        }

        std::map<std::string, std::vector<ArrayOf>> groupedChildren;
        stringVector childOrder;
        for (xmlNodePtr child = node->children; child != nullptr; child = child->next) {
            if (child->type != XML_ELEMENT_NODE) {
                continue;
            }
            std::string childName = xmlName(child);
            if (groupedChildren.find(childName) == groupedChildren.end()) {
                childOrder.push_back(childName);
            }
            groupedChildren[childName].push_back(xmlNodeToStruct(child, options));
        }

        std::string text = directText(node);
        if (fieldNames.empty() && groupedChildren.empty()) {
            return textToArray(text);
        }
        if (!text.empty()) {
            std::string textField = uniqueFieldName(fieldNames, "Text");
            fieldNames.push_back(textField);
            fieldValues << textToArray(text);
        }
        for (const std::string& childName : childOrder) {
            std::string name = uniqueFieldName(fieldNames, validFieldName(childName));
            fieldNames.push_back(name);
            fieldValues << makeRepeatedValue(groupedChildren[childName]);
        }
        return ArrayOf::structScalar(fieldNames, fieldValues);
    }
    //=============================================================================
    xmlNodePtr
    findFirstElementByName(xmlNodePtr node, const std::string& name)
    {
        for (xmlNodePtr current = node; current != nullptr; current = current->next) {
            if (current->type == XML_ELEMENT_NODE && xmlName(current) == name) {
                return current;
            }
            xmlNodePtr found = findFirstElementByName(current->children, name);
            if (found != nullptr) {
                return found;
            }
        }
        return nullptr;
    }
    //=============================================================================
    xmlNodePtr
    selectXPathNode(xmlDocPtr doc, const XmlStructReadOptions& options, std::wstring& errorMessage)
    {
        xmlXPathContextPtr context = xmlXPathNewContext(doc);
        if (context == nullptr) {
            errorMessage = L"Unable to create XPath context.";
            return nullptr;
        }
        for (const auto& ns : options.registeredNamespaces) {
            xmlXPathRegisterNs(context, BAD_CAST ns.first.c_str(), BAD_CAST ns.second.c_str());
        }
        std::string expression = wstring_to_utf8(options.structSelector);
        xmlXPathObjectPtr result = xmlXPathEvalExpression(BAD_CAST expression.c_str(), context);
        xmlXPathFreeContext(context);
        if (result == nullptr || result->nodesetval == nullptr || result->nodesetval->nodeNr < 1) {
            if (result != nullptr) {
                xmlXPathFreeObject(result);
            }
            errorMessage = L"StructSelector did not match an XML element.";
            return nullptr;
        }
        xmlNodePtr node = result->nodesetval->nodeTab[0];
        xmlXPathFreeObject(result);
        if (node == nullptr || node->type != XML_ELEMENT_NODE) {
            errorMessage = L"StructSelector did not match an XML element.";
            return nullptr;
        }
        return node;
    }
    //=============================================================================
    xmlNodePtr
    selectStructRoot(xmlDocPtr doc, const XmlStructReadOptions& options, std::wstring& errorMessage)
    {
        xmlNodePtr root = xmlDocGetRootElement(doc);
        if (root == nullptr) {
            errorMessage = L"XML document has no root element.";
            return nullptr;
        }
        if (!options.structSelector.empty()) {
            return selectXPathNode(doc, options, errorMessage);
        }
        if (!options.structNodeName.empty()) {
            xmlNodePtr found
                = findFirstElementByName(root, wstring_to_utf8(options.structNodeName));
            if (found == nullptr) {
                errorMessage = L"StructNodeName did not match an XML element.";
            }
            return found;
        }
        return root;
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOf
XmlReadStruct(
    const std::wstring& filename, const XmlStructReadOptions& options, std::wstring& errorMessage)
{
    if (!options.fileType.empty() && options.fileType != "xml") {
        errorMessage = L"Only XML FileType is supported by readstruct.";
        return ArrayOf::empty();
    }
    xmlDocPtr doc = XmlCorePrivate::parseXmlFile(filename, true, errorMessage);
    if (doc == nullptr) {
        return ArrayOf::empty();
    }
    xmlNodePtr node = selectStructRoot(doc, options, errorMessage);
    ArrayOf result = node == nullptr ? ArrayOf::empty() : xmlNodeToStruct(node, options);
    xmlFreeDoc(doc);
    return result;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

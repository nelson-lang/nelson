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
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    ArrayOf
    makeXmlElementObject(const std::string& text, const std::string& name)
    {
        stringVector names = { "Text", "Name" };
        ArrayOfVector values;
        values << ArrayOf::characterArray(text);
        values << ArrayOf::stringArray(name);
        return ArrayOf::classdefArray("xmlElement", names, values);
    }
    //=============================================================================
    ArrayOf
    makeXmlNodeListObject(const std::vector<ArrayOf>& nodes)
    {
        Dimensions dims(1, static_cast<indexType>(nodes.size()));
        auto* elements = static_cast<ArrayOf*>(
            ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount(), stringVector(), false));
        for (size_t k = 0; k < nodes.size(); k++) {
            elements[k] = nodes[k];
        }
        stringVector names = { "Nodes" };
        ArrayOfVector values;
        values << ArrayOf(NLS_CELL_ARRAY, dims, elements);
        return ArrayOf::classdefArray("xmlNodeList", names, values);
    }
    //=============================================================================
    bool
    isXmlElementObject(const ArrayOf& value)
    {
        return value.isClassType() && value.isScalar() && value.classType() == "xmlElement"
            && value.isField("Text") && value.isField("Name");
    }
    //=============================================================================
    bool
    isXmlNodeListObject(const ArrayOf& value)
    {
        return value.isClassType() && value.isScalar() && value.classType() == "xmlNodeList"
            && value.isField("Nodes");
    }
    //=============================================================================
    std::string
    nodeName(xmlNodePtr node)
    {
        if (node == nullptr || node->name == nullptr) {
            return "";
        }
        return reinterpret_cast<const char*>(node->name);
    }
    //=============================================================================
    bool
    serializeNode(xmlNodePtr node, std::string& text, std::wstring& errorMessage)
    {
        xmlDocPtr tempDoc = xmlNewDoc(BAD_CAST "1.0");
        if (tempDoc == nullptr) {
            errorMessage = L"Unable to serialize XML node.";
            return false;
        }
        xmlNodePtr copy = xmlDocCopyNode(node, tempDoc, 1);
        if (copy == nullptr) {
            xmlFreeDoc(tempDoc);
            errorMessage = L"Unable to serialize XML node.";
            return false;
        }
        xmlDocSetRootElement(tempDoc, copy);
        xmlReconciliateNs(tempDoc, copy);
        xmlBufferPtr buffer = xmlBufferCreate();
        if (buffer == nullptr) {
            xmlFreeDoc(tempDoc);
            errorMessage = L"Unable to serialize XML node.";
            return false;
        }
        int result = xmlNodeDump(buffer, tempDoc, copy, 0, 0);
        if (result < 0) {
            xmlBufferFree(buffer);
            xmlFreeDoc(tempDoc);
            errorMessage = L"Unable to serialize XML node.";
            return false;
        }
        text.assign(reinterpret_cast<const char*>(xmlBufferContent(buffer)),
            static_cast<size_t>(xmlBufferLength(buffer)));
        xmlBufferFree(buffer);
        xmlFreeDoc(tempDoc);
        return true;
    }
    //=============================================================================
    xmlDocPtr
    parseXmlObject(const ArrayOf& value, std::wstring& errorMessage)
    {
        std::wstring text;
        if (XmlCorePrivate::isXmlDocumentObject(value)) {
            text = value.field("Text").asWideString();
        } else if (isXmlElementObject(value)) {
            text = value.field("Text").asWideString();
        } else {
            errorMessage = L"XML document or element expected.";
            return nullptr;
        }
        std::string data = wstring_to_utf8(text);
        return XmlCorePrivate::parseXmlMemory(
            data, nullptr, "UTF-8", XML_PARSE_NONET, errorMessage, L"Unable to parse XML node.");
    }
    //=============================================================================
    void
    collectElements(xmlNodePtr node, const std::string& tagName, std::vector<xmlNodePtr>& matches)
    {
        for (xmlNodePtr current = node; current != nullptr; current = current->next) {
            if (current->type == XML_ELEMENT_NODE
                && (tagName == "*" || nodeName(current) == tagName)) {
                matches.push_back(current);
            }
            collectElements(current->children, tagName, matches);
        }
    }
    //=============================================================================
    ArrayOf
    getDocumentElement(const ArrayOfVector& args, std::wstring& errorMessage)
    {
        if (args.size() != 1) {
            errorMessage = L"One XML document expected.";
            return ArrayOf::empty();
        }
        xmlDocPtr doc = parseXmlObject(args[0], errorMessage);
        if (doc == nullptr) {
            return ArrayOf::empty();
        }
        xmlNodePtr root = xmlDocGetRootElement(doc);
        if (root == nullptr) {
            xmlFreeDoc(doc);
            return ArrayOf::empty();
        }
        std::string text;
        std::string name = nodeName(root);
        bool ok = serializeNode(root, text, errorMessage);
        xmlFreeDoc(doc);
        return ok ? makeXmlElementObject(text, name) : ArrayOf::empty();
    }
    //=============================================================================
    ArrayOf
    getElementsByTagName(const ArrayOfVector& args, std::wstring& errorMessage)
    {
        if (args.size() != 2) {
            errorMessage = L"XML document or element and tag name expected.";
            return ArrayOf::empty();
        }
        xmlDocPtr doc = parseXmlObject(args[0], errorMessage);
        if (doc == nullptr) {
            return ArrayOf::empty();
        }
        std::string tagName = wstring_to_utf8(args[1].asWideString());
        std::vector<xmlNodePtr> matches;
        xmlNodePtr root = xmlDocGetRootElement(doc);
        collectElements(isXmlElementObject(args[0]) && root != nullptr ? root->children : root,
            tagName, matches);
        std::vector<ArrayOf> nodes;
        nodes.reserve(matches.size());
        for (xmlNodePtr node : matches) {
            std::string text;
            if (!serializeNode(node, text, errorMessage)) {
                xmlFreeDoc(doc);
                return ArrayOf::empty();
            }
            nodes.push_back(makeXmlElementObject(text, nodeName(node)));
        }
        xmlFreeDoc(doc);
        return makeXmlNodeListObject(nodes);
    }
    //=============================================================================
    ArrayOf
    getTextContent(const ArrayOfVector& args, std::wstring& errorMessage)
    {
        if (args.size() != 1) {
            errorMessage = L"One XML document or element expected.";
            return ArrayOf::empty();
        }
        xmlDocPtr doc = parseXmlObject(args[0], errorMessage);
        if (doc == nullptr) {
            return ArrayOf::empty();
        }
        xmlChar* content = xmlNodeGetContent(xmlDocGetRootElement(doc));
        std::string text = content == nullptr ? "" : reinterpret_cast<const char*>(content);
        if (content != nullptr) {
            xmlFree(content);
        }
        xmlFreeDoc(doc);
        return ArrayOf::characterArray(text);
    }
    //=============================================================================
    ArrayOf
    attribute(const ArrayOfVector& args, std::wstring& errorMessage)
    {
        if (args.size() != 2) {
            errorMessage = L"XML element and attribute name expected.";
            return ArrayOf::empty();
        }
        xmlDocPtr doc = parseXmlObject(args[0], errorMessage);
        if (doc == nullptr) {
            return ArrayOf::empty();
        }
        std::string attrName = wstring_to_utf8(args[1].asWideString());
        xmlChar* value = xmlGetProp(xmlDocGetRootElement(doc), BAD_CAST attrName.c_str());
        std::string text = value == nullptr ? "" : reinterpret_cast<const char*>(value);
        if (value != nullptr) {
            xmlFree(value);
        }
        xmlFreeDoc(doc);
        return ArrayOf::characterArray(text);
    }
    //=============================================================================
    ArrayOf
    hasAttribute(const ArrayOfVector& args, std::wstring& errorMessage)
    {
        if (args.size() != 2) {
            errorMessage = L"XML element and attribute name expected.";
            return ArrayOf::empty();
        }
        xmlDocPtr doc = parseXmlObject(args[0], errorMessage);
        if (doc == nullptr) {
            return ArrayOf::empty();
        }
        std::string attrName = wstring_to_utf8(args[1].asWideString());
        xmlChar* value = xmlGetProp(xmlDocGetRootElement(doc), BAD_CAST attrName.c_str());
        bool exists = value != nullptr;
        if (value != nullptr) {
            xmlFree(value);
        }
        xmlFreeDoc(doc);
        return ArrayOf::logicalScalar(exists);
    }
    //=============================================================================
    ArrayOf
    length(const ArrayOfVector& args, std::wstring& errorMessage)
    {
        if (args.size() != 1 || !isXmlNodeListObject(args[0])) {
            errorMessage = L"XML node list expected.";
            return ArrayOf::empty();
        }
        return ArrayOf::doubleScalar(static_cast<double>(args[0].field("Nodes").elementCount()));
    }
    //=============================================================================
    ArrayOf
    item(const ArrayOfVector& args, std::wstring& errorMessage)
    {
        if (args.size() != 2 || !isXmlNodeListObject(args[0])) {
            errorMessage = L"XML node list and index expected.";
            return ArrayOf::empty();
        }
        double indexValue = args[1].asDoubleScalar();
        if (indexValue < 0) {
            return ArrayOf::empty();
        }
        ArrayOf nodes = args[0].field("Nodes");
        indexType index = static_cast<indexType>(indexValue);
        if (static_cast<double>(index) != indexValue || index >= nodes.elementCount()) {
            return ArrayOf::empty();
        }
        auto* elements = static_cast<const ArrayOf*>(nodes.dataPointer());
        return elements[index];
    }
    //=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
XmlDomMethod(const std::string& method, const ArrayOfVector& args, std::wstring& errorMessage)
{
    ArrayOf result;
    if (method == "getDocumentElement") {
        result = getDocumentElement(args, errorMessage);
    } else if (method == "getElementsByTagName") {
        result = getElementsByTagName(args, errorMessage);
    } else if (method == "getTagName" || method == "getNodeName") {
        if (args.size() == 1 && isXmlElementObject(args[0])) {
            result = args[0].field("Name");
        } else {
            errorMessage = L"XML element expected.";
        }
    } else if (method == "getTextContent") {
        result = getTextContent(args, errorMessage);
    } else if (method == "getAttribute") {
        result = attribute(args, errorMessage);
    } else if (method == "hasAttribute") {
        result = hasAttribute(args, errorMessage);
    } else if (method == "getLength") {
        result = length(args, errorMessage);
    } else if (method == "item") {
        result = item(args, errorMessage);
    } else {
        errorMessage = L"XML DOM method not supported.";
    }
    ArrayOfVector output;
    if (errorMessage.empty()) {
        output << result;
    }
    return output;
}
//=============================================================================
} // namespace Nelson
//=============================================================================

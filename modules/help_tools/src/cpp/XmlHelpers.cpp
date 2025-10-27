//=============================================================================
#include "XmlHelpers.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
void
addRawXml(xmlNodePtr parent, const std::string& rawxml)
{

    xmlNodePtr list = nullptr;

    int ret = xmlParseInNodeContext(parent, rawxml.c_str(), (int)rawxml.size(), 0, &list);

    if (ret == 0 && list) {
        for (xmlNodePtr cur = list; cur; cur = cur->next) {
            xmlAddChild(parent, xmlCopyNode(cur, 1));
        }
        xmlFreeNodeList(list);
    }
}
//=============================================================================
}
//=============================================================================

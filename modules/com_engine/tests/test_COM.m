%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WINDOWS ONLY-->
%=============================================================================
% write a xml file
xmlDoc = actxserver('Microsoft.XMLDOM');
assert_isequal(class(xmlDoc), 'COM.MSXML2DOMDocument');
%=============================================================================
% create root element
oRoot = invoke(xmlDoc, 'createElement', 'Root');
assert_isequal(class(oRoot), 'COM.MSXML2IXMLDOMElement');
r = invoke(xmlDoc, 'appendChild', oRoot);
assert_isequal(class(r), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
% add element
oElement = invoke(xmlDoc, 'selectSingleNode', 'Root');
assert_isequal(class(oElement), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
% create a child
oElement = invoke(xmlDoc, 'createElement' , 'element');
assert_isequal(class(oElement), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
pdocumentElement = xmlDoc.documentElement;
assert_isequal(class(pdocumentElement), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
% add child to root element
r = invoke(pdocumentElement, 'appendChild', oElement);
assert_isequal(class(r), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
oName = invoke(xmlDoc,'createElement','name');
assert_isequal(class(oName), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
oName.Text = 'CORNET';
r = invoke(oElement, 'appendChild', oName);
assert_isequal(class(r), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
oFirstname = invoke(xmlDoc,'createElement','firstname');
assert_isequal(class(oFirstname), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
set(oFirstname,'Text', 'Allan');
r = invoke(oElement, 'appendChild', oFirstname);
assert_isequal(class(r), 'COM.MSXML2IXMLDOMElement');
%=============================================================================
delete(COM_used());
%=============================================================================

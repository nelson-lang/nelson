%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('xmlread'), -2);
asserts.isequal(nargout('xmlread'), 2);
asserts.isequal(nargin('xmlwrite'), -1);
asserts.isequal(nargout('xmlwrite'), -1);
xml_name_completion = completion('xml');
asserts.isfalse(any(strcmp(xml_name_completion.macros, 'xmlDocument')));
asserts.isfalse(any(strcmp(xml_name_completion.macros, 'xmlElement')));
asserts.isfalse(any(strcmp(xml_name_completion.macros, 'xmlNodeList')));
asserts.isfalse(any(startsWith(xml_name_completion.macros, '@xml')));
asserts.istrue(any(strcmp(xml_name_completion.builtin, 'xmlread')));
asserts.istrue(any(strcmp(xml_name_completion.builtin, 'xmlwrite')));
asserts.isfalse(any(startsWith(xml_name_completion.builtin, '__xml')));
asserts.isfalse(any(startsWith(xml_name_completion.builtin, '__xmldoc')));
%=============================================================================
xml_filename = [modulepath('xml'), '/tests/test_xml.xml'];
doc = xmlread(xml_filename);
asserts.isequal(class(doc), 'xmlDocument');
doc_methods = methods(doc);
asserts.istrue(any(strcmp(doc_methods, 'getDocumentElement')));
asserts.istrue(any(strcmp(doc_methods, 'getElementsByTagName')));
asserts.istrue(any(strcmp(doc_methods, 'getTextContent')));
asserts.istrue(ismethod(doc, 'getDocumentElement'));
[doc_with_parser, parser] = xmlread(xml_filename);
asserts.isequal(class(doc_with_parser), 'xmlDocument');
asserts.istrue(isempty(parser));
doc_engine = xmlread(xml_filename, 'XMLEngine', 'AUTO');
asserts.isequal(class(doc_engine), 'xmlDocument');
doc_display = evalc('display(doc, ''doc'')');
asserts.istrue(contains(doc_display, 'doc ='));
asserts.istrue(contains(doc_display, '[#document: null]'));
asserts.isfalse(contains(doc_display, 'with properties'));
doc_disp = evalc('disp(doc)');
asserts.istrue(contains(doc_disp, '[#document: null]'));
asserts.isfalse(contains(doc_disp, 'Text:'));
doc_completion = completion('doc.');
asserts.istrue(doc_completion.showpopup);
asserts.istrue(any(strcmp(doc_completion.methods, 'getDocumentElement')));
asserts.istrue(any(strcmp(doc_completion.methods, 'getElementsByTagName')));
asserts.istrue(any(strcmp(doc_completion.methods, 'getTextContent')));
root = doc.getDocumentElement();
asserts.isequal(class(root), 'xmlElement');
root_methods = methods(root);
asserts.istrue(any(strcmp(root_methods, 'getTagName')));
asserts.istrue(any(strcmp(root_methods, 'getAttribute')));
asserts.istrue(ismethod(root, 'getTextContent'));
asserts.isequal(root.getTagName(), "table");
asserts.isequal(getDocumentElement(doc).getNodeName(), "table");
root_display = evalc('disp(root)');
asserts.istrue(contains(root_display, '[table: null]'));
asserts.istrue(contains(root.getTextContent(), 'NY'));
root_completion = completion('root.');
asserts.istrue(root_completion.showpopup);
asserts.istrue(any(strcmp(root_completion.methods, 'getTagName')));
asserts.istrue(any(strcmp(root_completion.methods, 'getAttribute')));
asserts.istrue(any(strcmp(root_completion.methods, 'hasAttribute')));
nodes = doc.getElementsByTagName('row');
asserts.isequal(class(nodes), 'xmlNodeList');
nodes_methods = methods(nodes);
asserts.istrue(any(strcmp(nodes_methods, 'getLength')));
asserts.istrue(any(strcmp(nodes_methods, 'item')));
asserts.istrue(ismethod(nodes, 'item'));
asserts.isequal(nodes.getLength(), 3);
nodes_completion = completion('nodes.');
asserts.istrue(nodes_completion.showpopup);
asserts.istrue(any(strcmp(nodes_completion.methods, 'getLength')));
asserts.istrue(any(strcmp(nodes_completion.methods, 'item')));
asserts.isequal(doc.getElementsByTagName('table').getLength(), 1);
asserts.isequal(root.getElementsByTagName('table').getLength(), 0);
first_row = nodes.item(0);
asserts.isequal(class(first_row), 'xmlElement');
asserts.isequal(first_row.getTagName(), "row");
asserts.isequal(first_row.getAttribute('missing'), '');
asserts.isfalse(first_row.hasAttribute('missing'));
asserts.istrue(contains(first_row.getTextContent(), 'NY'));
asserts.istrue(isempty(nodes.item(3)));
txt = xmlwrite(doc);
asserts.istrue(contains(txt, '<table'));
%=============================================================================
out_filename = [tempname(), '.xml'];
xmlwrite(out_filename, doc);
asserts.istrue(isfile(out_filename));
doc2 = xmlread(out_filename);
asserts.istrue(contains(xmlwrite(doc2), '<table'));
%=============================================================================
doctype_filename = [tempname(), '.xml'];
doctype_txt = ['<?xml version="1.0" encoding="UTF-8"?>', newline(), ...
               '<!DOCTYPE root [<!ELEMENT root (#PCDATA)>]>', newline(), ...
               '<root>value</root>'];
filewrite(doctype_filename, doctype_txt);
asserts.checkerror('xmlread(doctype_filename, ''AllowDoctype'', false)', _('DOCTYPE declarations are not permitted.'));
doctype_doc = xmlread(doctype_filename, 'AllowDoctype', true);
asserts.isequal(class(doctype_doc), 'xmlDocument');
%=============================================================================
xml_text_filename = [tempname(), '.xml'];
xmlwrite(xml_text_filename, '<root><empty/></root>');
xml_text = fileread(xml_text_filename);
asserts.istrue(contains(xml_text, '<empty></empty>'));
asserts.isfalse(contains(xml_text, '<empty/>'));
%=============================================================================
catalog_filename = [tempname(), '.xml'];
catalog_text = ['<catalog>', ...
                '<book id="b1"><title>Numerical Methods</title><year>2026</year></book>', ...
                '<book id="b2"><title>Signal Processing</title><year>2025</year></book>', ...
                '</catalog>'];
filewrite(catalog_filename, catalog_text);
catalog_doc = xmlread(catalog_filename);
catalog_doc_completion = completion('catalog_doc.');
asserts.istrue(any(strcmp(catalog_doc_completion.methods, 'getDocumentElement')));
catalog_root = catalog_doc.getDocumentElement();
catalog_root_completion = completion('catalog_root.get');
asserts.istrue(catalog_root_completion.showpopup);
asserts.isequal(catalog_root_completion.prefix, 'get');
asserts.istrue(any(strcmp(catalog_root_completion.methods, 'getTagName')));
asserts.istrue(any(strcmp(catalog_root_completion.methods, 'getElementsByTagName')));
books = catalog_doc.getElementsByTagName('book');
books_completion = completion('books.i');
asserts.istrue(books_completion.showpopup);
asserts.isequal(books_completion.prefix, 'i');
asserts.istrue(any(strcmp(books_completion.methods, 'item')));
%=============================================================================
asserts.checkerror('xmlwrite(out_filename, ''<root>'')', _('Unable to parse XML DOM node.'));
asserts.checkerror('xmlread(xml_filename, ''XMLEngine'', ''bad'')', _('XMLEngine not supported.'));
asserts.checkerror('xmlread(xml_filename, ''XMLEngine'', ''bad'')', _('XMLEngine not supported.'), 'Nelson:xmlread:engineNotSupported');
%=============================================================================
asserts.checkerror('xmlread([modulepath(''xml''), ''/tests/test_xml.xml''], ''AllowDoctype'', false, ''Bad'', true)', _('Property name not supported.'));
asserts.checkerror('xmlread([modulepath(''xml''), ''/tests/test_xml.xml''], ''AllowDoctype'', false, ''Bad'', true)', _('Property name not supported.'), 'Nelson:xml:propertyNotSupported');
asserts.checkerror('xmlread(xml_filename, ''AllowDoctype'')', _('Name-value arguments must be paired.'));
asserts.checkerror('xmlread(xml_filename, 1, true)', _('Invalid parameter name. The parameter name must be a nonempty string or character array.'));
%=============================================================================

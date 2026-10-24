%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('readstruct'), -2);
asserts.isequal(nargout('readstruct'), 1);
asserts.isequal(nargin('writestruct'), -3);
asserts.isequal(nargout('writestruct'), 0);
%=============================================================================
filename = [tempname(), '.xml'];
txt = ['<?xml version="1.0" encoding="UTF-8"?>', newline(), ...
       '<dataset version="1">', newline(), ...
       '  <item id="a">10</item>', newline(), ...
       '  <item id="b">20</item>', newline(), ...
       '  <name>Nelson</name>', newline(), ...
       '</dataset>'];
filewrite(filename, txt);
%=============================================================================
s = readstruct(filename, 'FileType', 'xml');
asserts.istrue(isfield(s, 'versionAttribute'));
asserts.istrue(isfield(s, 'item'));
asserts.istrue(isfield(s, 'name'));
asserts.isequal(s.name, "Nelson");
asserts.isequal(s.item(1).idAttribute, "a");
asserts.isequal(s.item(1).Text, 10);
%=============================================================================
selected = readstruct(filename, 'StructNodeName', 'name');
asserts.isequal(selected, "Nelson");
%=============================================================================
asserts.checkerror('readstruct(filename, ''StructSelector'', ''//missing'')', ...
                  _('StructSelector did not match an XML element.'));
%=============================================================================
without_attributes = readstruct(filename, 'ImportAttributes', false);
asserts.isfalse(isfield(without_attributes, 'versionAttribute'));
date_locale = readstruct(filename, 'DateLocale', 'en_US');
asserts.isequal(date_locale.name, "Nelson");
%=============================================================================
ns_filename = [tempname(), '.xml'];
ns_txt = ['<?xml version="1.0" encoding="UTF-8"?>', newline(), ...
          '<root xmlns:n="urn:nelson">', newline(), ...
          '  <n:item code="N"><n:name><![CDATA[Nelson XML]]></n:name></n:item>', newline(), ...
          '</root>'];
filewrite(ns_filename, ns_txt);
ns = readstruct(ns_filename, 'StructSelector', '//n:item', ...
                'RegisteredNamespaces', {'n', 'urn:nelson'});
asserts.isequal(ns.codeAttribute, "N");
asserts.isequal(ns.n_name, "Nelson XML");
%=============================================================================
default_ns_filename = [tempname(), '.xml'];
default_ns_txt = ['<?xml version="1.0" encoding="UTF-8"?>', newline(), ...
                  '<root xmlns="urn:root">', newline(), ...
                  '  <item id="a">1</item>', newline(), ...
                  '  <item id="b">2</item>', newline(), ...
                  '</root>'];
filewrite(default_ns_filename, default_ns_txt);
default_ns = readstruct(default_ns_filename, 'StructSelector', '//d:item', ...
                        'RegisteredNamespaces', {'d', 'urn:root'});
asserts.isequal(default_ns.idAttribute, "a");
asserts.isequal(default_ns.Text, 1);
%=============================================================================
invalid_names_filename = [tempname(), '.xml'];
invalid_names_txt = ['<root xmlns:x="urn:x">', ...
                     '<a-b>1</a-b><a.b>2</a.b><a_b>3</a_b><_x>4</_x><x:y>5</x:y>', ...
                     '</root>'];
filewrite(invalid_names_filename, invalid_names_txt);
invalid_names = readstruct(invalid_names_filename);
asserts.istrue(isfield(invalid_names, 'a_b'));
asserts.istrue(isfield(invalid_names, 'a_b_1'));
asserts.istrue(isfield(invalid_names, 'a_b_2'));
asserts.istrue(isfield(invalid_names, 'x_x'));
asserts.istrue(isfield(invalid_names, 'x_y'));
asserts.isequal(invalid_names.x_y, 5);
%=============================================================================
attribute_ns_filename = [tempname(), '.xml'];
attribute_ns_txt = '<root xmlns:n="urn:n" n:code="A"><item n:id="B">1</item></root>';
filewrite(attribute_ns_filename, attribute_ns_txt);
attribute_ns = readstruct(attribute_ns_filename);
asserts.isequal(attribute_ns.xmlns_nAttribute, "urn:n");
asserts.isequal(attribute_ns.n_codeAttribute, "A");
asserts.isequal(attribute_ns.item.n_idAttribute, "B");
attribute_ns_without_attributes = readstruct(attribute_ns_filename, 'ImportAttributes', false);
asserts.isfalse(isfield(attribute_ns_without_attributes, 'xmlns_nAttribute'));
asserts.isfalse(isfield(attribute_ns_without_attributes, 'n_codeAttribute'));
%=============================================================================
default_ns_attribute_filename = [tempname(), '.xml'];
default_ns_attribute_txt = '<root xmlns="urn:def"><item>1</item></root>';
filewrite(default_ns_attribute_filename, default_ns_attribute_txt);
default_ns_attribute = readstruct(default_ns_attribute_filename);
asserts.isequal(default_ns_attribute.xmlnsAttribute, "urn:def");
%=============================================================================
empty_repeat_filename = [tempname(), '.xml'];
empty_repeat_txt = '<root><item/><item></item><item>3</item></root>';
filewrite(empty_repeat_filename, empty_repeat_txt);
empty_repeat = readstruct(empty_repeat_filename);
asserts.istrue(isnan(empty_repeat.item(1)));
asserts.istrue(isnan(empty_repeat.item(2)));
asserts.isequal(empty_repeat.item(3), 3);
%=============================================================================
nonuniform_read_filename = [tempname(), '.xml'];
nonuniform_read_txt = '<root><mixed><a>1</a></mixed><mixed><b>2</b></mixed></root>';
filewrite(nonuniform_read_filename, nonuniform_read_txt);
nonuniform_read = readstruct(nonuniform_read_filename);
asserts.istrue(isstruct(nonuniform_read.mixed));
asserts.isequal(size(nonuniform_read.mixed), [1 2]);
asserts.istrue(isfield(nonuniform_read.mixed, 'a'));
asserts.istrue(isfield(nonuniform_read.mixed, 'b'));
asserts.isequal(nonuniform_read.mixed(1).a, 1);
asserts.isequal(class(nonuniform_read.mixed(1).b), 'missing');
asserts.isequal(class(nonuniform_read.mixed(2).a), 'missing');
asserts.isequal(nonuniform_read.mixed(2).b, 2);
nonuniform_read_out_filename = [tempname(), '.xml'];
writestruct(nonuniform_read, nonuniform_read_out_filename, 'StructNodeName', 'root');
nonuniform_read_text = fileread(nonuniform_read_out_filename);
asserts.istrue(contains(nonuniform_read_text, '<mixed>'));
asserts.istrue(contains(nonuniform_read_text, '<a>1</a>'));
asserts.istrue(contains(nonuniform_read_text, '<b>2</b>'));
asserts.isfalse(contains(nonuniform_read_text, '<a/>'));
asserts.isfalse(contains(nonuniform_read_text, '<b/>'));
%=============================================================================
types_filename = [tempname(), '.xml'];
types_txt = ['<?xml version="1.0" encoding="UTF-8"?>', newline(), ...
             '<root><flag>true</flag><item>10</item><item>text</item><item>20</item></root>'];
filewrite(types_filename, types_txt);
types = readstruct(types_filename);
asserts.isequal(types.flag, "true");
asserts.isequal(types.item(1), 10);
asserts.istrue(isnan(types.item(2)));
asserts.isequal(types.item(3), 20);
types_out_filename = [tempname(), '.xml'];
writestruct(types, types_out_filename, 'StructNodeName', 'root');
types_text = fileread(types_out_filename);
asserts.istrue(contains(types_text, '<flag>true</flag>'));
asserts.istrue(contains(types_text, '<item>10</item>'));
asserts.istrue(contains(types_text, '<item></item>'));
asserts.isfalse(contains(types_text, '<item/>'));
asserts.istrue(contains(types_text, '<item>20</item>'));
types_compact_filename = [tempname(), '.xml'];
writestruct(types, types_compact_filename, 'StructNodeName', 'root', 'PrettyPrint', false);
types_compact_text = fileread(types_compact_filename);
asserts.istrue(contains(types_compact_text, '<item></item>'));
asserts.isfalse(contains(types_compact_text, '<item/>'));
asserts.isfalse(contains(types_compact_text, [newline(), '    <item>']));
%=============================================================================
strings_filename = [tempname(), '.xml'];
strings_txt = '<root><name>a</name><name>b</name></root>';
filewrite(strings_filename, strings_txt);
strings = readstruct(strings_filename);
strings_out_filename = [tempname(), '.xml'];
writestruct(strings, strings_out_filename, 'StructNodeName', 'root');
strings_text = fileread(strings_out_filename);
asserts.istrue(contains(strings_text, '<name>a</name>'));
asserts.istrue(contains(strings_text, '<name>b</name>'));
%=============================================================================
finite_values = struct();
finite_values.item = [Inf, -Inf];
finite_filename = [tempname(), '.xml'];
writestruct(finite_values, finite_filename, 'StructNodeName', 'root');
finite_text = fileread(finite_filename);
asserts.istrue(contains(finite_text, '<item>Inf</item>'));
asserts.istrue(contains(finite_text, '<item>-Inf</item>'));
%=============================================================================
nonuniform = struct();
nonuniform.x = struct('a', 1);
nonuniform.x(2).b = 2;
nonuniform_filename = [tempname(), '.xml'];
writestruct(nonuniform, nonuniform_filename, 'StructNodeName', 'root');
nonuniform_text = fileread(nonuniform_filename);
asserts.istrue(contains(nonuniform_text, '<x>'));
asserts.istrue(contains(nonuniform_text, '<a>1</a>'));
asserts.istrue(contains(nonuniform_text, '<b>2</b>'));
%=============================================================================
custom_suffix = struct();
custom_suffix.idAttr = "A";
custom_suffix.value = 7;
custom_suffix_filename = [tempname(), '.xml'];
writestruct(custom_suffix, custom_suffix_filename, 'StructNodeName', 'root', ...
            'AttributeSuffix', 'Attr');
custom_suffix_text = fileread(custom_suffix_filename);
asserts.istrue(contains(custom_suffix_text, '<root id="A">'));
asserts.istrue(contains(custom_suffix_text, [newline(), '    <value>7</value>']));
%=============================================================================
missing_field = struct();
missing_field.present = 1;
missing_field.omitted = missing();
missing_field_filename = [tempname(), '.xml'];
writestruct(missing_field, missing_field_filename, 'StructNodeName', 'root');
missing_field_text = fileread(missing_field_filename);
asserts.istrue(contains(missing_field_text, '<present>1</present>'));
asserts.isfalse(contains(missing_field_text, '<omitted>'));
%=============================================================================
out_filename = [tempname(), '.xml'];
writestruct(s, out_filename, 'StructNodeName', 'dataset', 'PrettyPrint', true);
asserts.istrue(isfile(out_filename));
roundtrip = readstruct(out_filename);
asserts.istrue(isfield(roundtrip, 'item'));
asserts.isequal(roundtrip.name, "Nelson");
%=============================================================================
cell_struct = struct();
cell_struct.values = {'a', 'b'};
cell_filename = [tempname(), '.xml'];
writestruct(cell_struct, cell_filename, 'StructNodeName', 'root');
cell_text = fileread(cell_filename);
asserts.istrue(contains(cell_text, '<item>a</item>'));
asserts.istrue(contains(cell_text, '<item>b</item>'));
%=============================================================================
asserts.checkerror('readstruct(filename, ''FileType'', ''json'')', _('Only XML FileType is supported by readstruct.'));
asserts.checkerror('writestruct(s, out_filename, ''FileType'', ''json'')', _('Only XML FileType is supported by writestruct.'));
asserts.checkerror('readstruct(filename, ''RegisteredNamespaces'', {''n''})', _('RegisteredNamespaces must contain prefix and URL pairs.'));
asserts.checkerror('readstruct(filename, ''RegisteredNamespaces'', {''n''})', _('RegisteredNamespaces must contain prefix and URL pairs.'), 'Nelson:readstruct:invalidRegisteredNamespaces');
asserts.checkerror('readstruct(filename, ''FileType'')', _('Name-value arguments must be paired.'));
asserts.checkerror('readstruct(filename, ''FileType'')', _('Name-value arguments must be paired.'), 'Nelson:xml:nameValueNotPaired');
asserts.checkerror('writestruct(s, out_filename, ''FileType'')', _('Name-value arguments must be paired.'));
%=============================================================================

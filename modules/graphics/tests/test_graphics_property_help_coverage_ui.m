%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
skip_testsuite(~isfile([modulepath('graphics'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
close all
helpRoot = [modulepath('graphics', 'root'), '/help'];
f = figure('Visible', 'off', 'Position', [100 100 500 400]);
ax = axes('Parent', f);
check_properties_documented(helpRoot, 'uicontrol', uicontrol('Parent', f, 'Style', 'pushbutton', 'String', 'ok'));
check_properties_documented(helpRoot, 'uicontextmenu', uicontextmenu(f));
check_properties_documented(helpRoot, 'uimenu', uimenu(f, 'Text', 'Menu'));
check_properties_documented(helpRoot, 'annotation.line', annotation(f, 'line'));
check_properties_documented(helpRoot, 'annotation.arrow', annotation(f, 'arrow'));
check_properties_documented(helpRoot, 'annotation.doublearrow', annotation(f, 'doublearrow'));
check_properties_documented(helpRoot, 'annotation.textarrow', annotation(f, 'textarrow'));
check_properties_documented(helpRoot, 'annotation.rectangle', annotation(f, 'rectangle'));
check_properties_documented(helpRoot, 'annotation.ellipse', annotation(f, 'ellipse'));
check_properties_documented(helpRoot, 'annotation.textbox', annotation(f, 'textbox'));
close all
%=============================================================================
%=============================================================================
function check_properties_documented(helpRoot, objectName, h)
  check_properties_documented_in_language(helpRoot, 'en_US', objectName, h);
  check_properties_documented_in_language(helpRoot, 'fr_FR', objectName, h);
end
%=============================================================================
function check_properties_documented_in_language(helpRoot, language, objectName, h)
  chapterFilename = find_help_file(helpRoot, language, 'chapter.xml', '4_properties');
  asserts.istrue(isfile(chapterFilename));
  indexFilename = find_help_file(helpRoot, language, 'nelson.graphics.properties.xml', '');
  asserts.istrue(isfile(indexFilename));
  filename = find_help_file(helpRoot, language, ['nelson.graphics.', objectName, '.properties.xml'], '');
  asserts.istrue(isfile(filename));
  helpText = fileread(filename);
  asserts.istrue(contains(helpText, '<keyword>'));
  asserts.istrue(contains(helpText, 'properties</keyword>'));
  asserts.isfalse(contains(helpText, '<syntax>'));
  asserts.isfalse(contains(helpText, '<param_input>'));
  asserts.isfalse(contains(helpText, '<param_output>'));
  if strcmp(language, 'fr_FR')
    asserts.istrue(contains(helpText, '<th>Propriete</th>'));
    asserts.istrue(contains(helpText, '<th>Type et valeurs prises en charge</th>'));
  else
    asserts.istrue(contains(helpText, '<th>Property</th>'));
    asserts.istrue(contains(helpText, '<th>Type and supported values</th>'));
  end
  asserts.istrue(contains(helpText, '<th>Action</th>'));
  asserts.istrue(property_table_columns_are_ordered(helpText, language));
  names = properties(h);
  asserts.isfalse(contains(helpText, 'Rendering or interaction property'));
  asserts.isfalse(contains(helpText, 'Graphics property exposed'));
  asserts.isfalse(contains(helpText, 'Data, labels'));
  asserts.isfalse(contains(helpText, 'Metadata, object relationship'));
  asserts.isfalse(contains(helpText, 'Geometry, camera'));
  asserts.isfalse(contains(helpText, 'property-specific Nelson value'));
  asserts.isfalse(contains(helpText, 'value class and shape defined'));
  asserts.isfalse(contains(helpText, 'class et forme de valeur definies'));
  asserts.isfalse(contains(helpText, 'object-specific'));
  asserts.isfalse(contains(helpText, 'when supported'));
  asserts.isfalse(contains(helpText, 'accepted by the object'));
  asserts.isfalse(contains(helpText, 'accepted by this property'));
  asserts.isfalse(contains(helpText, 'accepted by the property'));
  asserts.isfalse(contains(helpText, 'property setter'));
  asserts.isfalse(contains(helpText, 'validator'));
  asserts.istrue(count(helpText, 'Type:') >= numel(names));
  if strcmp(language, 'fr_FR')
    asserts.istrue(count(helpText, 'Valeurs prises en charge:') >= numel(names));
  else
    asserts.istrue(count(helpText, 'Supported values:') >= numel(names));
  end
  for k = 1:numel(names)
    name = names{k};
    asserts.istrue(contains(helpText, ['<td><b>', name, '</b></td>']));
  end
  asserts.isfalse(contains(helpText, '<b>Action</b>'));
  asserts.istrue(count(helpText, '<td>') >= 3 * numel(names));
  constructors = constructor_functions_for_object(objectName);
  for k = 1:numel(constructors)
    asserts.istrue(contains(helpText, ['/', constructors{k}, '"']));
  end
  asserts.isfalse(contains(helpText, '<![CDATA[names = properties(h)]]>'));
  constructorInExample = false;
  for k = 1:numel(constructors)
    constructorInExample = constructorInExample || contains(helpText, [constructors{k}, '(']);
  end
  asserts.istrue(constructorInExample);
  check_function_help_links_to_property_page(helpRoot, language, objectName);
end
%=============================================================================
function tf = property_table_columns_are_ordered(helpText, language)
  if strcmp(language, 'fr_FR')
    header = '<th>Propriete</th>';
    typeHeader = '<th>Type et valeurs prises en charge</th>';
  else
    header = '<th>Property</th>';
    typeHeader = '<th>Type and supported values</th>';
  end
  p1 = strfind(helpText, header);
  p2 = strfind(helpText, '<th>Action</th>');
  p3 = strfind(helpText, typeHeader);
  tf = ~isempty(p1) && ~isempty(p2) && ~isempty(p3) && p1(1) < p2(1) && p2(1) < p3(1);
end
%=============================================================================
function constructors = constructor_functions_for_object(objectName)
  switch objectName
    case 'groot'
      constructors = {'groot'};
    case 'figure'
      constructors = {'figure'};
    case 'axes'
      constructors = {'axes'};
    case 'polaraxes'
      constructors = {'polaraxes'};
    case 'line'
      constructors = {'line', 'plot', 'plot3'};
    case 'stem'
      constructors = {'stem', 'stem3'};
    case 'errorbar'
      constructors = {'errorbar'};
    case 'patch'
      constructors = {'patch', 'fill', 'fill3'};
    case 'scatter'
      constructors = {'scatter', 'scatter3'};
    case 'bubblechart'
      constructors = {'bubblechart', 'bubblechart3'};
    case 'bubblelegend'
      constructors = {'bubblelegend'};
    case 'surface'
      constructors = {'surface', 'surf', 'mesh'};
    case 'functionsurface'
      constructors = {'fsurf', 'fmesh'};
    case {'functionline', 'parameterizedfunctionline'}
      constructors = {'fplot'};
    case 'functioncontour'
      constructors = {'fcontour'};
    case 'implicitfunctionline'
      constructors = {'fimplicit'};
    case 'implicitfunctionsurface'
      constructors = {'fimplicit3'};
    case 'image'
      constructors = {'image', 'imagesc', 'imshow'};
    case 'bar'
      constructors = {'bar', 'barh', 'bar3', 'bar3h'};
    case 'quiver'
      constructors = {'quiver', 'quiver3'};
    case 'contour'
      constructors = {'contour', 'contourf', 'contour3'};
    case {'annotation.line', 'annotation.arrow', 'annotation.doublearrow', 'annotation.textarrow', ...
        'annotation.rectangle', 'annotation.ellipse', 'annotation.textbox'}
      constructors = {'annotation'};
    otherwise
      constructors = {objectName};
  end
end
%=============================================================================
function check_function_help_links_to_property_page(helpRoot, language, objectName)
  helpPageName = primary_help_page_for_object(objectName);
  propertyPageObjectName = property_page_object_name_for_function_link(objectName);
  filename = find_help_file(helpRoot, language, [helpPageName, '.xml'], '');
  asserts.istrue(isfile(filename));
  helpText = fileread(filename);
  linkEnd = ['${graphics}2_graphics_objects/4_properties/nelson.graphics.', propertyPageObjectName, '.properties'];
  asserts.istrue(contains(helpText, ['<see_also>']));
  asserts.istrue(contains(helpText, ['linkend="', linkEnd, '"']));
  asserts.istrue(count(helpText, ['nelson.graphics.', propertyPageObjectName, '.properties']) >= 2);
  asserts.isfalse(contains(helpText, '<th>Property</th>'));
  asserts.isfalse(contains(helpText, '<th>Propri'));
  asserts.isfalse(contains(helpText, '<b>Supported'));
end
%=============================================================================
function filename = find_help_file(helpRoot, language, basename, requiredFolder)
  xmlRoot = [helpRoot, '/', language, '/xml'];
  filename = find_help_file_recursively(xmlRoot, basename, requiredFolder);
end
%=============================================================================
function filename = find_help_file_recursively(folder, basename, requiredFolder)
  filename = '';
  entries = dir(folder);
  for k = 1:numel(entries)
    name = entries(k).name;
    if strcmp(name, '.') || strcmp(name, '..')
      continue;
    end
    entryPath = [folder, '/', name];
    if entries(k).isdir
      filename = find_help_file_recursively(entryPath, basename, requiredFolder);
      if ~isempty(filename)
        return;
      end
    elseif strcmp(name, basename)
      normalizedFolder = strrep(folder, '\', '/');
      if isempty(requiredFolder) || contains(normalizedFolder, requiredFolder)
        filename = entryPath;
        return;
      end
    end
  end
end
%=============================================================================
function helpPageName = primary_help_page_for_object(objectName)
  if is_annotation_property_page(objectName)
    helpPageName = 'annotation';
    return
  end
  switch objectName
    case {'functionline', 'parameterizedfunctionline'}
      helpPageName = 'fplot';
    case 'functionsurface'
      helpPageName = 'fsurf';
    case 'functioncontour'
      helpPageName = 'fcontour';
    case 'implicitfunctionline'
      helpPageName = 'fimplicit';
    case 'implicitfunctionsurface'
      helpPageName = 'fimplicit3';
    otherwise
      helpPageName = objectName;
  end
end
%=============================================================================
function propertyPageObjectName = property_page_object_name_for_function_link(objectName)
  if is_annotation_property_page(objectName)
    propertyPageObjectName = 'annotation';
  else
    propertyPageObjectName = objectName;
  end
end
%=============================================================================
function tf = is_annotation_property_page(objectName)
  tf = strncmp(objectName, 'annotation.', 11);
end
%=============================================================================

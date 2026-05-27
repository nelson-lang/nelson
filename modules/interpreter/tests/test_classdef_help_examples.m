%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
testHelpersPath = modulepath('interpreter', 'tests');
addpath(testHelpersPath);
%=============================================================================
files = {
  [modulepath('data_structures'), '/help/en_US/xml/fieldnames.xml'];
  [modulepath('data_structures'), '/help/en_US/xml/struct.xml'];
  [modulepath('data_structures'), '/help/fr_FR/xml/fieldnames.xml'];
  [modulepath('data_structures'), '/help/fr_FR/xml/struct.xml'];
  [modulepath('console'), '/help/en_US/xml/completion.xml'];
  [modulepath('console'), '/help/fr_FR/xml/completion.xml'];
  [modulepath('interpreter'), '/help/en_US/xml/classdef.xml'];
  [modulepath('interpreter'), '/help/fr_FR/xml/classdef.xml'];
  [modulepath('memory_manager'), '/help/en_US/xml/clear.xml'];
  [modulepath('memory_manager'), '/help/fr_FR/xml/clear.xml'];
  [modulepath('stream_manager'), '/help/en_US/xml/load.xml'];
  [modulepath('stream_manager'), '/help/en_US/xml/save.xml'];
  [modulepath('stream_manager'), '/help/fr_FR/xml/load.xml'];
  [modulepath('stream_manager'), '/help/fr_FR/xml/save.xml'];
  [modulepath('types'), '/help/en_US/xml/class.xml'];
  [modulepath('types'), '/help/en_US/xml/isa.xml'];
  [modulepath('types'), '/help/en_US/xml/isobject.xml'];
  [modulepath('types'), '/help/fr_FR/xml/class.xml'];
  [modulepath('types'), '/help/fr_FR/xml/isa.xml'];
  [modulepath('types'), '/help/fr_FR/xml/isobject.xml'];
  [modulepath('handle'), '/help/en_US/xml/addlistener.xml'];
  [modulepath('handle'), '/help/en_US/xml/delete.xml'];
  [modulepath('handle'), '/help/en_US/xml/enumeration.xml'];
  [modulepath('handle'), '/help/en_US/xml/events.xml'];
  [modulepath('handle'), '/help/en_US/xml/listener.xml'];
  [modulepath('handle'), '/help/en_US/xml/metaclass.xml'];
  [modulepath('handle'), '/help/en_US/xml/notify.xml'];
  [modulepath('handle'), '/help/en_US/xml/ismethod.xml'];
  [modulepath('handle'), '/help/en_US/xml/isprop.xml'];
  [modulepath('handle'), '/help/en_US/xml/methods.xml'];
  [modulepath('handle'), '/help/en_US/xml/properties.xml'];
  [modulepath('hdf5'), '/help/en_US/xml/h5read.xml'];
  [modulepath('hdf5'), '/help/en_US/xml/h5write.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/addlistener.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/delete.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/enumeration.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/events.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/listener.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/metaclass.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/notify.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/ismethod.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/isprop.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/methods.xml'];
  [modulepath('handle'), '/help/fr_FR/xml/properties.xml'];
  [modulepath('hdf5'), '/help/fr_FR/xml/h5read.xml'];
  [modulepath('hdf5'), '/help/fr_FR/xml/h5write.xml']
};
%=============================================================================
startMarker = '<![CDATA[';
endMarker = ']]>';
%=============================================================================
for k = 1:length(files)
  filename = files{k};
  xml = fileread(filename);
  cursor = 1;
  exampleCount = 0;
  while cursor <= length(xml)
    relStart = strfind(xml(cursor:length(xml)), startMarker);
    if isempty(relStart)
      break;
    end
    startIndex = cursor + relStart(1) - 1 + length(startMarker);
    relEnd = strfind(xml(startIndex:length(xml)), endMarker);
    assert_isfalse(isempty(relEnd));
    endIndex = startIndex + relEnd(1) - 2;
    code = xml(startIndex:endIndex);
    try
      runClassdefHelpExample(code);
    catch e
      disp(filename);
      disp(e.message);
      assert_istrue(false);
    end
    exampleCount = exampleCount + 1;
    cursor = endIndex + length(endMarker) + 1;
  end
  assert_istrue(exampleCount > 0);
end
%=============================================================================
rmpath(testHelpersPath);
%=============================================================================

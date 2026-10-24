%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
if ~ismodule('compiler')
  addmodule(fullfile(nelsonroot(), 'modules', 'compiler'), 'compiler');
end
directory = tempname();
mkdir(directory);
cleanup = onCleanup(@() rmdir(directory, 's'));
entry = fullfile(directory, 'nc_callback_entry.m');
helper = fullfile(directory, 'nc_text_callback.m');
write_lines(helper, {'function nc_text_callback(varargin)', 'end'});
dynamicCases = {
  'uicontrol(''CreateFcn'', ''nc_text_callback;'');'
  'uicontrol(''Callback'', "nc_text_callback;");'
  'uicontrol(''callback'', callback);'
  'timer(''TimerFcn'', ''nc_text_callback;'');'
  'set(h, ''CloseRequestFcn'', ''nc_text_callback;'');'
  'set(groot(), ''DefaultFigureCreateFcn'', ''nc_text_callback;'');'
  'set(h, property, callback);'
  'set(h, ''Visible'', ''on'', property, callback);'
  'set(h, properties);'
  'set(h, [prefix, ''cn''], callback);'
  'property = ''Tag''; property = ''Callback''; set(h, property, callback);'
  'property = ''Tag''; [property, unused] = deal(''Callback'', 1); set(h, property, callback);'
  'h.Callback = ''nc_text_callback;'';'
  'h.(''Callback'') = ''nc_text_callback;'';'
  'h.(property) = callback;'
};
for k = 1:numel(dynamicCases)
  write_lines(entry, {'function nc_callback_entry(h, callback, property)', dynamicCases{k}, 'end'});
  plan = jsondecode(__nelsonc_plan__('link', entry));
  asserts.istrue(plan.complete);
  asserts.istrue(plan.dynamicFallback);
  asserts.istrue(any(endsWith(string({plan.files.source}), '/nc_text_callback.m')));
  asserts.istrue(all([plan.modules.complete]));
end
staticCases = {
  'uicontrol(''Callback'', @nc_text_callback);'
  'uicontrol(''Callback'', {@nc_text_callback, 42});'
  'uicontrol(''Callback'', @(h, e) nc_text_callback(h, e));'
  'set(h, ''Callback'', @nc_text_callback);'
  'h.Callback = @nc_text_callback;'
  'h.(''Callback'') = @nc_text_callback;'
};
for k = 1:numel(staticCases)
  write_lines(entry, {'function nc_callback_entry(h)', staticCases{k}, 'end'});
  plan = jsondecode(__nelsonc_plan__('link', entry));
  asserts.istrue(plan.complete);
  asserts.isfalse(plan.dynamicFallback);
  asserts.istrue(any(endsWith(string({plan.files.source}), '/nc_text_callback.m')));
end
clearCases = {
  'uicontrol(''Callback'', '''');'
  'uicontrol(''Callback'', "");'
  'uicontrol(''Callback'', []);'
  'h.Callback = [];'
  'set(h, ''Callback'');'
  'set(h, ''String'', ''Callback'', ''Visible'', ''off'');'
  'set(h, [axisName, ''Tick''], ticks);'
  'set(h, [axisName, ''TickLabel''], labels);'
  'property = [axisName, ''Tick'']; set(h, property, ticks);'
  'property = [axisName, ''TickLabel'']; set(h, property, labels);'
  'h.([prefix, ''Setting'']) = value;'
  'strcmp(''Callback'', ''nc_text_callback'');'
};
for k = 1:numel(clearCases)
  write_lines(entry, {'function nc_callback_entry(h)', clearCases{k}, 'end'});
  plan = jsondecode(__nelsonc_plan__('link', entry));
  asserts.istrue(plan.complete);
  asserts.isfalse(plan.dynamicFallback);
  asserts.isequal(numel(plan.files), 1);
end
write_lines(entry, {'function value = nc_callback_entry()', ...
  'value = uicontrol(''Callback'', ''plain data'');', 'end', ...
  'function value = uicontrol(varargin)', 'value = 1;', 'end'});
plan = jsondecode(__nelsonc_plan__('link', entry));
asserts.istrue(plan.complete);
asserts.isfalse(plan.dynamicFallback);
asserts.isfalse(plan.requiresGraphics);
clear cleanup;

function write_lines(filename, lines)
  fid = fopen(filename, 'wt');
  fprintf(fid, '%s\n', lines{:});
  fclose(fid);
end

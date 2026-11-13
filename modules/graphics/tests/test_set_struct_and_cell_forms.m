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
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 600 400]);
%=============================================================================
% set(h, s): struct fields are property names.
ax = axes(f);
set(ax, struct('Units', 'pixels'));
assert_isequal(ax.Units, 'pixels');
%=============================================================================
% Fields apply in order: Position before Units is then converted.
ax = axes(f);
s = struct();
s.Position = [10 20 200 100];
s.Units = 'pixels';
set(ax, s);
assert_isequal(ax.Units, 'pixels');
assert_isequal(ax.Position, [6001 8001 120000 40000]);
ax = axes(f);
s = struct();
s.Units = 'pixels';
s.Position = [10 20 200 100];
set(ax, s);
assert_isequal(ax.Position, [10 20 200 100]);
%=============================================================================
% Field names are not case sensitive.
ax = axes(f);
set(ax, struct('units', 'pixels', 'BOX', 'on'));
assert_isequal(ax.Units, 'pixels');
assert_isequal(char(ax.Box), 'on');
%=============================================================================
% Struct arrays apply each element in turn: the last one wins.
ax = axes(f);
set(ax, struct('Units', {'pixels', 'points'}));
assert_isequal(ax.Units, 'points');
ax = axes(f);
set(ax, struct('Units', cell(1, 0)));
assert_isequal(ax.Units, 'normalized');
set(ax, struct());
set(ax, struct([]));
assert_isequal(ax.Units, 'normalized');
%=============================================================================
% A cell field value is passed as is.
ax = axes(f);
set(ax, struct('UserData', {{1, 2}}));
assert_isequal(ax.UserData, {1, 2});
%=============================================================================
% Struct applies to every handle and mixes with name/value pairs.
a = axes(f);
b = axes(f);
set([a b], struct('Units', 'pixels'));
assert_isequal(a.Units, 'pixels');
assert_isequal(b.Units, 'pixels');
ax = axes(f);
set(ax, 'Units', 'pixels', struct('Position', [1 2 3 4]), 'Box', 'on');
assert_isequal(ax.Position, [1 2 3 4]);
assert_isequal(char(ax.Box), 'on');
%=============================================================================
% set(h, names, values) with cell arrays, in order.
ax = axes(f);
set(ax, {'Position', 'Units'}, {[10 20 200 100], 'pixels'});
assert_isequal(ax.Units, 'pixels');
assert_isequal(ax.Position, [6001 8001 120000 40000]);
ax = axes(f);
set(ax, {'Units', 'Position'}, {'pixels', [10 20 200 100]});
assert_isequal(ax.Position, [10 20 200 100]);
ax = axes(f);
set(ax, {'Units'; "Box"}, {'pixels', 'on'});
assert_isequal(ax.Units, 'pixels');
assert_isequal(char(ax.Box), 'on');
ax = axes(f);
set(ax, {'Units', 'Units'}, {'pixels', 'points'});
assert_isequal(ax.Units, 'points');
%=============================================================================
% One value row is shared, M rows give one row per handle.
a = axes(f);
b = axes(f);
set([a b], {'Units', 'Box'}, {'pixels', 'on'; 'normalized', 'off'});
assert_isequal(a.Units, 'pixels');
assert_isequal(char(a.Box), 'on');
assert_isequal(b.Units, 'normalized');
assert_isequal(char(b.Box), 'off');
set([a b], {'Units', 'Box'}, {'points', 'on'});
assert_isequal(a.Units, 'points');
assert_isequal(b.Units, 'points');
assert_isequal(char(b.Box), 'on');
%=============================================================================
% Cell form mixes with pairs and structs.
ax = axes(f);
set(ax, struct('Units', 'pixels'), {'Box'}, {'on'}, 'Color', [1 0 0]);
assert_isequal(ax.Units, 'pixels');
assert_isequal(char(ax.Box), 'on');
assert_isequal(ax.Color, [1 0 0]);
set(ax, {}, {});
%=============================================================================
% Errors.
ax = axes(f);
msg = _('Value cell array size does not match the property name cell array.');
assert_checkerror('set(ax, {''Units'', ''Box''}, {''pixels''})', msg, 'Nelson:set:nameValueCellMismatch');
assert_checkerror('set(ax, {''Units''}, {})', msg, 'Nelson:set:nameValueCellMismatch');
assert_checkerror('set(ax, {}, {1})', msg, 'Nelson:set:nameValueCellMismatch');
msg = _('Value cell array must have one row, or one row per handle.');
assert_checkerror('set(ax, {''Units''}, {''pixels''; ''points''; ''pixels''})', msg, 'Nelson:set:valueCellRows');
assert_checkerror('set(ax, {''Units'', ''Box''}, {''pixels''; ''on''})', msg, 'Nelson:set:valueCellRows');
assert_checkerror('set(ax, {''Units'', ''Box''}, cell(0, 2))', msg, 'Nelson:set:valueCellRows');
msg = _('Value cell array must hold one value per property for each handle.');
assert_checkerror('set([a b], {''Units'', ''Box''}, {''pixels''; ''on''})', msg, 'Nelson:set:valueCellPerProperty');
msg = _('Property names in a cell array must be non-empty character vectors or string scalars.');
assert_checkerror('set(ax, {''Units'', 1}, {''pixels'', 2})', msg, 'Nelson:set:invalidNameCell');
assert_checkerror('set(ax, {''''}, {1})', msg, 'Nelson:set:invalidNameCell');
msg = _('Property names and values must come in pairs.');
assert_checkerror('set(ax, {''Units''}, ''pixels'')', msg, 'Nelson:set:invalidPairs');
assert_checkerror('set(ax, struct(''Units'', ''pixels''), ''Box'')', msg, 'Nelson:set:invalidPairs');
assert_checkerror('set(ax, 5, 6)', msg, 'Nelson:set:invalidPairs');
msg = _('Expected property name/value pairs, a struct or a cell array of property names.');
assert_checkerror('set(ax, {''Units''})', msg, 'Nelson:set:invalidArgument');
assert_checkerror('set(ax, 5)', msg, 'Nelson:set:invalidArgument');
%=============================================================================
% A bad struct field reports the property error.
ax = axes(f);
try
  set(ax, struct('Units', 'foo'));
  assert_istrue(false);
catch e
  assert_isequal(e.identifier, 'Nelson:graphics:propertyError');
end
assert_isequal(ax.Units, 'normalized');
%=============================================================================
delete(f);
%=============================================================================

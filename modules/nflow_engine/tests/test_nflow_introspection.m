%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Object introspection: PortHandles and the Commented state.
%=============================================================================
bdclose('all');
new_system('m');
add_block('nflow/source/sine', 'm/Sine');
add_block('nflow/math/gain', 'm/Gain');
%=============================================================================
% PortHandles reports 1-based Inport/Outport indices from the catalog.
ph = get_param('m/Gain', 'PortHandles');
assert_istrue(isstruct(ph));
assert_istrue(isfield(ph, 'Inport'));
assert_istrue(isfield(ph, 'Outport'));
assert_isequal(ph.Inport, 1);
assert_isequal(ph.Outport, 1);
% A source has no input port.
phs = get_param('m/Sine', 'PortHandles');
assert_isequal(numel(phs.Inport), 0);
assert_isequal(phs.Outport, 1);
%=============================================================================
% Commented defaults to 'off' and accepts off/on/through.
assert_isequal(get_param('m/Gain', 'Commented'), 'off');
set_param('m/Gain', 'Commented', 'on');
assert_isequal(get_param('m/Gain', 'Commented'), 'on');
set_param('m/Gain', 'Commented', 'through');
assert_isequal(get_param('m/Gain', 'Commented'), 'through');
% An invalid value is rejected and leaves the state unchanged.
assert_checkerror('set_param(''m/Gain'', ''Commented'', ''maybe'')', ...
  _('Commented must be ''off'', ''on'' or ''through''.'));
assert_isequal(get_param('m/Gain', 'Commented'), 'through');
% PortHandles is read-only.
assert_checkerror('set_param(''m/Gain'', ''PortHandles'', 1)', ...
  _('Parameter ''PortHandles'' is read-only.'));
%=============================================================================
% Signal name on a line (nflow_lineName get/set).
add_line('m', 'Sine/1', 'Gain/1');
assert_isequal(nflow_lineName('m', 'Sine/1', 'Gain/1'), '');
nflow_lineName('m', 'Sine/1', 'Gain/1', 'drive');
assert_isequal(nflow_lineName('m', 'Sine/1', 'Gain/1'), 'drive');
% Naming a non-existent connection errors.
assert_checkerror('nflow_lineName(''m'', ''Gain/1'', ''Sine/1'', ''x'')', _('No such connection.'));
%=============================================================================
% The Commented state and the signal name survive save/load.
f = [tempname(), '.nflow'];
save_system('m', f);
bdclose('m');
load_system(f);
assert_isequal(get_param('m/Gain', 'Commented'), 'through');
assert_isequal(nflow_lineName('m', 'Sine/1', 'Gain/1'), 'drive');
bdclose('all');
rmfile(f);
%=============================================================================

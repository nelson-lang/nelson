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
asserts.isequal(nargin('new_system'), 1);
asserts.isequal(nargout('new_system'), 1);
%=============================================================================
% Target use case: build a model programmatically.
bdclose('all');
h = new_system('demo');
asserts.istrue(isa(h, 'double'));
asserts.istrue(bdIsLoaded('demo'));
add_block('nflow/source/sine', 'demo/Sine');
add_block('nflow/math/gain', 'demo/Gain', 'gain', 2);
add_block('nflow/sink/scope', 'demo/Scope');
add_line('demo', 'Sine/1', 'Gain/1');
add_line('demo', 'Gain/1', 'Scope/1');
set_param('demo', 'StopTime', 10);
%=============================================================================
blocks = get_param('demo', 'Blocks');
asserts.istrue(iscell(blocks));
asserts.isequal(numel(blocks), 3);
asserts.isequal(get_param('demo/Gain', 'gain'), '2');
asserts.isequal(get_param('demo', 'StopTime'), '10');
asserts.istrue(bdIsDirty('demo'));
%=============================================================================
% Auto-naming.
bdclose('all');
a = new_system();
b = new_system();
asserts.isequal(get_param(a, 'Name'), 'untitled');
asserts.isequal(get_param(b, 'Name'), 'untitled1');
bdclose('all');
%=============================================================================
% MakeNameUnique.
new_system('m');
add_block('nflow/math/gain', 'm/Gain');
p = add_block('nflow/math/gain', 'm/Gain', 'MakeNameUnique', 'on');
asserts.isequal(p, 'm/Gain1');
asserts.checkerror('add_block(''nflow/math/gain'', ''m/Gain'')', _('A block named ''Gain'' already exists.'));
bdclose('all');
%=============================================================================
% Occupied input and duplicate connections are rejected.
new_system('m');
add_block('nflow/source/sine', 'm/S1');
add_block('nflow/source/sine', 'm/S2');
add_block('nflow/math/gain', 'm/G');
add_line('m', 'S1/1', 'G/1');
asserts.checkerror('add_line(''m'', ''S2/1'', ''G/1'')', _('Input port 1 of ''G'' is already connected.'));
bdclose('all');
%=============================================================================
% Save / load round-trip.
new_system('rt');
add_block('nflow/math/gain', 'rt/Gain', 'gain', 5);
f = [tempname(), '.nflow'];
saved = save_system('rt', f);
asserts.istrue(isfile(saved));
asserts.isfalse(bdIsDirty('rt'));
bdclose('rt');
asserts.isfalse(bdIsLoaded('rt'));
load_system(saved);
asserts.istrue(bdIsLoaded('rt'));
asserts.isequal(get_param('rt/Gain', 'gain'), '5');
bdclose('all');
rmfile(f);
%=============================================================================
% Closing a dirty model without a flag errors.
new_system('d');
add_block('nflow/math/gain', 'd/Gain');
asserts.checkerror('close_system(''d'')', _('Unable to close the model ''d'' because it has been changed. To save changes, use close_system(model, 1). To discard changes, use close_system(model, 0).'));
close_system('d', 0);
asserts.isfalse(bdIsLoaded('d'));
%=============================================================================

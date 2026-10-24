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
bdclose('all');
new_system('m');
add_block('nflow/source/sine', 'm/Sine');
add_block('nflow/math/gain', 'm/Gain');
%=============================================================================
% NFlow.connectBlocks wires the first free ports.
NFlow.connectBlocks('m/Sine', 'm/Gain');
% A second connect to the same one-input block fails.
add_block('nflow/source/sine', 'm/Sine2');
asserts.checkerror('NFlow.connectBlocks(''m/Sine2'', ''m/Gain'')', _('You must connect to a block with at least one unconnected input port.'));
bdclose('all');
%=============================================================================
% delete_block cascades to connections.
new_system('m');
add_block('nflow/math/gain', 'm/A');
add_block('nflow/math/gain', 'm/B');
add_line('m', 'A/1', 'B/1');
delete_block('m/A');
asserts.checkerror('get_param(''m/A'', ''gain'')', _('No block named ''A''.'));
% Deleting the block removed the connection; a fresh connection is possible.
add_block('nflow/math/gain', 'm/C');
add_line('m', 'C/1', 'B/1');
bdclose('all');
%=============================================================================
% set_param is atomic: an invalid pair leaves the model unchanged.
new_system('m');
add_block('nflow/math/gain', 'm/Gain', 'gain', 1);
asserts.checkerror('set_param(''m'', ''StopTime'', 5, ''NoSuchParam'', 1)', _('Unknown model parameter ''NoSuchParam''.'));
asserts.isequal(get_param('m', 'StopTime'), '10');
bdclose('all');
%=============================================================================
% Model parameters map to the reference names.
new_system('m');
set_param('m', 'SampleTime', 0.05);
set_param('m', 'AutoRoute', 'off');
asserts.isequal(get_param('m', 'SampleTime'), '0.05');
asserts.isequal(get_param('m', 'AutoRoute'), 'off');
asserts.isequal(get_param('m', 'Dirty'), 'on');
asserts.checkerror('set_param(''m'', ''Dirty'', ''off'')', _('Parameter ''Dirty'' is read-only.'));
bdclose('all');
%=============================================================================

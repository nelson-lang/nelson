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
% Programmatic layout: nflow_layout arranges a (sub)system into
% deterministic left-to-right layers by connectivity, at a single depth.
%=============================================================================
bdclose('all');
new_system('m');
add_block('nflow/source/sine', 'm/S');
add_block('nflow/math/gain', 'm/G1');
add_block('nflow/math/gain', 'm/G2');
add_line('m', 'S/1', 'G1/1');
add_line('m', 'S/1', 'G2/1'); % fan-out: G1 and G2 share layer 1
%=============================================================================
nflow_layout('m');
pS = get_param('m/S', 'Position');
pG1 = get_param('m/G1', 'Position');
pG2 = get_param('m/G2', 'Position');
% The source is left of its successors.
assert_istrue(pS(1) < pG1(1));
assert_istrue(pS(1) < pG2(1));
% Same layer -> same column x, different rows (no overlap).
assert_isequal(pG1(1), pG2(1));
assert_istrue(pG1(2) ~= pG2(2));
%=============================================================================
% Layout is deterministic: running it again yields identical positions.
nflow_layout('m');
assert_isequal(get_param('m/G1', 'Position'), pG1);
assert_isequal(get_param('m/G2', 'Position'), pG2);
%=============================================================================
% Layout targets a single depth: a nested subsystem is arranged independently.
add_block('subsystem', 'm/Sub');
add_block('nflow/source/sine', 'm/Sub/IS');
add_block('nflow/math/gain', 'm/Sub/IG');
add_line('m', 'Sub/IS/1', 'Sub/IG/1');
nflow_layout('m/Sub');
piS = get_param('m/Sub/IS', 'Position');
piG = get_param('m/Sub/IG', 'Position');
assert_istrue(piS(1) < piG(1));
%=============================================================================
% An unknown system path is rejected.
assert_checkerror('nflow_layout(''m/Nope'')', _('No block named ''Nope''.'));
%=============================================================================
bdclose('all');
%=============================================================================

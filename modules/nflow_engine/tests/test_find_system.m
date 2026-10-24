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
new_system('fsmodel');
add_block('nflow/source/sine', 'fsmodel/Sine');
add_block('nflow/math/gain', 'fsmodel/Gain', 'gain', 2);
add_block('nflow/math/gain', 'fsmodel/Gain2', 'gain', 3);
add_block('nflow/sink/toWorkspace', 'fsmodel/Out', 'VariableName', 'y');
%=============================================================================
% find_system(sys) returns the system and every block path under it.
all = find_system('fsmodel');
asserts.istrue(iscell(all));
asserts.istrue(any(strcmp(all, 'fsmodel')));
asserts.istrue(any(strcmp(all, 'fsmodel/Sine')));
asserts.istrue(any(strcmp(all, 'fsmodel/Gain')));
asserts.istrue(any(strcmp(all, 'fsmodel/Gain2')));
asserts.istrue(any(strcmp(all, 'fsmodel/Out')));
asserts.isequal(numel(all), 5);
%=============================================================================
% find_system(sys, 'BlockType', T) returns only the matching blocks.
gains = find_system('fsmodel', 'BlockType', 'gain');
asserts.istrue(iscell(gains));
asserts.isequal(numel(gains), 2);
asserts.istrue(any(strcmp(gains, 'fsmodel/Gain')));
asserts.istrue(any(strcmp(gains, 'fsmodel/Gain2')));
% The system itself is omitted when a BlockType filter is given.
asserts.isfalse(any(strcmp(gains, 'fsmodel')));
%=============================================================================
% A non-matching type yields an empty result.
none = find_system('fsmodel', 'BlockType', 'nosuchtype');
asserts.isequal(numel(none), 0);
%=============================================================================
% An unknown property is rejected.
asserts.checkerror('find_system(''fsmodel'', ''Nope'', 1)', _('Unknown find_system property ''Nope''.'));
%=============================================================================
% Hierarchical search: a subsystem with nested blocks.
add_block('subsystem', 'fsmodel/Nest');
add_block('nflow/math/gain', 'fsmodel/Nest/InnerA', 'gain', 4);
add_block('nflow/source/sine', 'fsmodel/Nest/InnerB');
% Unlimited search reaches nested paths.
allh = find_system('fsmodel');
asserts.istrue(any(strcmp(allh, 'fsmodel/Nest')));
asserts.istrue(any(strcmp(allh, 'fsmodel/Nest/InnerA')));
asserts.istrue(any(strcmp(allh, 'fsmodel/Nest/InnerB')));
% SearchDepth 0 = the system only.
d0 = find_system('fsmodel', 'SearchDepth', 0);
asserts.isequal(d0, {'fsmodel'});
% SearchDepth 1 = the system and its direct children (no nested contents).
d1 = find_system('fsmodel', 'SearchDepth', 1);
asserts.istrue(any(strcmp(d1, 'fsmodel/Nest')));
asserts.isfalse(any(strcmp(d1, 'fsmodel/Nest/InnerA')));
% SearchDepth 2 reaches one level of nesting.
d2 = find_system('fsmodel', 'SearchDepth', 2);
asserts.istrue(any(strcmp(d2, 'fsmodel/Nest/InnerA')));
% A BlockType filter still searches every level.
gains2 = find_system('fsmodel', 'BlockType', 'gain');
asserts.isequal(numel(gains2), 3); % Gain, Gain2, Nest/InnerA
asserts.istrue(any(strcmp(gains2, 'fsmodel/Nest/InnerA')));
% Depth-first order is deterministic: a nested child follows its parent.
idxNest = find(strcmp(allh, 'fsmodel/Nest'));
idxInner = find(strcmp(allh, 'fsmodel/Nest/InnerA'));
asserts.istrue(idxInner > idxNest);
%=============================================================================
bdclose('fsmodel');
%=============================================================================

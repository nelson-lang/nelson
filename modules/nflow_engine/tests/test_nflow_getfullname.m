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
% getfullname returns the full path named by a handle: a block handle yields
% 'model/block', a model handle yields 'model'. A char path is returned
% unchanged. A cell array of handles yields a cell array of paths. It is the
% inverse of getNFlowBlockHandle.
%=============================================================================
bdclose('all');
new_system('nfd');
add_block('nflow/math/gain', 'nfd/G1');
add_block('nflow/math/gain', 'nfd/G2');
%=============================================================================
% A block handle maps back to its full path.
h1 = getNFlowBlockHandle('nfd/G1');
assert_isequal(getfullname(h1), 'nfd/G1');
%=============================================================================
% Inverse relationship with getNFlowBlockHandle.
assert_isequal(getNFlowBlockHandle(getfullname(h1)), h1);
%=============================================================================
% A model handle maps back to the model name.
mh = get_param('nfd', 'Handle');
assert_isequal(getfullname(mh), 'nfd');
%=============================================================================
% A char path is already a full name and is returned unchanged.
assert_isequal(getfullname('nfd/G1'), 'nfd/G1');
%=============================================================================
% A cell array of handles yields a cell array of paths.
r = getfullname({getNFlowBlockHandle('nfd/G1'), getNFlowBlockHandle('nfd/G2')});
assert_istrue(iscell(r));
assert_isequal(r, {'nfd/G1', 'nfd/G2'});
%=============================================================================
% An invalid handle raises an error.
assert_checkerror('getfullname(-1)', _('Invalid nflow object handle.'));
%=============================================================================
bdclose('all');
%=============================================================================

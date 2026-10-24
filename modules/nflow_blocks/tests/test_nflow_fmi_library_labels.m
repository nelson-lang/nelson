%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
% The palette distinguishes the two FMI modes while preserving block types.
manifestPath = [modulepath('nflow_blocks', 'root'), '/libraries/fmi/library.json'];
library = jsondecode(fileread(manifestPath));
blocks = library.blocks;
if ~iscell(blocks)
  blocks = num2cell(blocks);
end
types = cellfun(@(block) block.type, blocks, 'UniformOutput', false);
cs = find(strcmp(types, 'fmu'));
me = find(strcmp(types, 'fmuMe'));
assert_isequal(numel(cs), 1);
assert_isequal(numel(me), 1);
assert_isequal(blocks{cs}.label, 'FMU (CS)');
assert_isequal(blocks{me}.label, 'FMU (ME)');
%=============================================================================

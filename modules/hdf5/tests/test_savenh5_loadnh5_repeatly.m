%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
test_nh5_file = [tempdir(), 'test_nh5saveload.nh5'];
if isfile(test_nh5_file)
  rmfile(test_nh5_file);
end
%=============================================================================
for i = [1:10]
  savenh5(test_nh5_file);
  loadnh5(test_nh5_file);
end
AA = 3;
savenh5(test_nh5_file,'-append','AA');
clear AA
loadnh5(test_nh5_file);
assert_isequal(AA, 3);
%=============================================================================

%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/224
% <-- Short Description -->
% cd 當第一個按讚的人 crashed Nelson.
%=============================================================================
cd(tempdir());
if ~isdir('當第一個按讚的人')
  mkdir('當第一個按讚的人')
end
cd 當第一個按讚的人
R = pwd();
assert_istrue(endsWith(R, '當第一個按讚的人'));
%=============================================================================
cd(tempdir());
 cd       當第一個按讚的人
R = pwd();
assert_istrue(endsWith(R, '當第一個按讚的人'));
%=============================================================================
cd(tempdir());
%=============================================================================
cd('當第一個按讚的人');
R = pwd();
assert_istrue(endsWith(R, '當第一個按讚的人'));
%=============================================================================

%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function embedded_data_entry()
  directory = fileparts(mfilename('fullpath'));
  native = loadnh5(fullfile(directory, 'values.nh5'));
  portable = loadmat(fullfile(directory, 'values.mat'));
  if ~isequal(native.A, portable.A)
    error('Embedded datasets differ.');
  end
  fprintf('EMBEDDED_SUM=%g\n', sum(native.A(:)));
end

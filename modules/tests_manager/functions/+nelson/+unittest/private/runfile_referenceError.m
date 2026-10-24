%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function msg = runfile_referenceError(filename, dia_ref, output_test)
  msg = {};
  expected_output = replace(fileread(dia_ref), char([13 10]), char(10));
  output_test = replace(output_test(:)', char([13 10]), char(10));
  if strcmp(expected_output, output_test)
    return
  end
  [~, f, ~] = fileparts(filename); %#ok<NLS0004>
  dia_res = [unittest_tempdir(), '', f, '.res'];
  if isfile(dia_res)
    rmfile(dia_res);
  end
  fileID = fopen(dia_res, 'wt');
  fwrite(fileID, output_test, 'char');
  fclose(fileID);
  res = diff_file(dia_ref, dia_res, false);
  msg = {'output res and ref are not equal.', dia_res, dia_ref, res};
end
%=============================================================================

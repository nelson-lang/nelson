%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function text = __nelsonc_help_text__(filename)
  if ~isfile(filename)
    error('Nelson:nelsonc:helpFile:applicationHelpFileExpected', 'Application help must be an existing UTF-8 file.');
  end
  fid = fopen(filename, 'rb');
  if fid < 0
    error('Nelson:nelsonc:helpFile:cannotOpenApplicationHelpFile', 'Cannot open application help file.');
  end
  cleanup = onCleanup(@() fclose(fid));
  bytes = fread(fid, 1048577, '*uint8')';
  [message, number] = ferror(fid);
  if number ~= 0
    error('Nelson:nelsonc:helpFile:cannotReadApplicationHelp', 'Cannot read application help: %s', message);
  end
  clear cleanup;
  if numel(bytes) > 1048576 || any(bytes == 0)
    error('Nelson:nelsonc:helpFile:applicationHelpTooLarge', 'Application help must not exceed 1 MiB or contain null characters.');
  end
  try
    text = native2unicode(bytes, 'UTF-8');
    if ~isequal(unicode2native(text, 'UTF-8'), bytes)
      error('Invalid UTF-8.');
    end
  catch
    error('Nelson:nelsonc:helpFile:applicationHelpInvalidUtf8', 'Application help must contain valid UTF-8 text.');
  end
  if ~isempty(text) && double(text(1)) == 65279
    text = text(2:end);
  end
end

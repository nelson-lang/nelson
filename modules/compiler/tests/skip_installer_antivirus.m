%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Shared helper for the Windows installer tests (not a test itself: no test_
% prefix, so the runner does not collect it).
%
%   try
%     ... build / package / deploy an installer ...
%   catch err
%     skip_installer_antivirus(err);
%   end
%
% Re-raises every genuine failure, but turns an antivirus false positive on the
% freshly compiled (unsigned) launcher into a skip: real-time protection may
% abort the Inno Setup compile or lock the launcher while its resources are
% copied, which is environmental and must not fail the suite.
%=============================================================================
function skip_installer_antivirus(err)
  signatures = {'nelson:nelsonc:installertool:antivirusblocked', ...
    'potentially unwanted', 'potentiellement', 'virus', 'installer resources'};
  haystack = lower([err.identifier, ' ', err.message]);
  blocked = false;
  for k = 1:numel(signatures)
    if contains(haystack, signatures{k})
      blocked = true;
      break;
    end
  end
  skip_testsuite(blocked, 'Installer compilation blocked by antivirus (false positive)');
  rethrow(err);
end
%=============================================================================

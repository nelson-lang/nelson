%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = nmm_is_remote_registry(source)
  % A remote registry is fetched with websave (raw bytes) and must be signed:
  % http(s) endpoints, plus file:// URLs (same code path, testable offline).
  tf = ischar(source) && (startsWith(source, 'http://') ...
    || startsWith(source, 'https://') || startsWith(source, 'file://'));
end
%=============================================================================

%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_config(varargin)
  if length(varargin) ~= 0
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  registry = nmm_registry_source();
  cachedPackages = nmm_cache('list');
  report = struct();
  report.registry = registry;
  report.cache_dir = nmm_cache('dir');
  report.usermodulesdir = usermodulesdir();
  report.signing_key_configured = ~isempty(getenv('NELSON_NMM_REGISTRY_SIGNING_KEY'));
  report.trusted_keys = length(nmm_registry_signature('trusted-keys'));
  report.allow_unsigned_registry = strcmp(strtrim(getenv('NELSON_NMM_REGISTRY_ALLOW_UNSIGNED')), '1');
  report.offline_cache_count = length(cachedPackages);
end
%=============================================================================

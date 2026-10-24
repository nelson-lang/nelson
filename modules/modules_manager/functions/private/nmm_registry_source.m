%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function source = nmm_registry_source()
  % Resolve the package registry Nelson reads, in order of precedence:
  %   1. NELSON_NMM_REGISTRY (explicit override: a file path or an http(s) URL);
  %   2. a registry.json created locally in usermodulesdir(), when it exists;
  %   3. the official Nelson package registry (signed, verified against the
  %      public key embedded in nmm_registry_trusted_keys).
  % So a fresh install browses and installs packages with no configuration,
  % while a local or private registry still takes precedence when set up.
  source = strtrim(getenv('NELSON_NMM_REGISTRY'));
  if ~isempty(source)
    return
  end
  local = [usermodulesdir(), 'registry.json'];
  if isfile(local)
    source = local;
    return
  end
  source = 'https://raw.githubusercontent.com/nelson-lang/nelson-modules-registry/master/registry.json';
end
%=============================================================================

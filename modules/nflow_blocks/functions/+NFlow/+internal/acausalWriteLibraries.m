%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% dirs = NFlow.internal.acausalWriteLibraries([libRoot])
%
% Acausal palette N5: materialise the acausal palettes in the CANONICAL nflow
% layout: one library per domain under
%   <nflow_blocks>/libraries/acausal_<domain>/library.json
%   <nflow_blocks>/libraries/acausal_<domain>/exports/<Name>.svg
% exactly like the builtin libraries (continuous, math, ...). The manifest
% (id "builtin.acausal_<domain>") comes from NFlow.internal.acausalGenerateLibrary and
% the icons from NFlow.internal.acausalGenerateIcon. Register the ids in the GUI loader
% + config to enable the palettes. Regenerable; re-run after catalog/icon edits.
%
% libRoot defaults to <nflow_blocks>/libraries.
%=============================================================================
function dirs = acausalWriteLibraries(libRoot)
  if nargin < 1 || isempty(libRoot)
    libRoot = [modulepath('nflow_blocks', 'root'), '/libraries'];
  end
  catalog = NFlow.internal.acausalCatalog();
  domains = unique({catalog.domain});
  dirs = {};
  for i = 1:numel(domains)
    domain = domains{i};
    libDir = [libRoot, '/acausal_', domain];
    expDir = [libDir, '/exports'];
    if ~isdir(expDir)
      mkdir(expDir);
    end
    % Manifest.
    manifest = NFlow.internal.acausalGenerateLibrary(domain);
    p = [libDir, '/library.json'];
    fid = fopen(p, 'w');
    if fid < 0
      error('Nelson:nflow:acausalWriteLib', 'Cannot write ''%s''.', p);
    end
    fwrite(fid, manifest);
    fclose(fid);
    % Icons, one per block of this domain, into exports/.
    for k = 1:numel(catalog)
      if ~strcmp(catalog(k).domain, domain)
        continue
      end
      svg = NFlow.internal.acausalGenerateIcon(catalog(k).name);
      ip = [expDir, '/', catalog(k).name, '.svg'];
      fid = fopen(ip, 'w');
      fwrite(fid, svg);
      fclose(fid);
    end
    dirs{end + 1} = libDir; %#ok<AGROW>
  end
end
%=============================================================================

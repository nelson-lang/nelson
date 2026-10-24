%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% files = NFlow.internal.causalWriteIcons([libRoot])
%
% Writes the palette icon of every causal block that has a Modelica.Blocks
% equivalent (the entries of libraries/msl-icons.json carrying a library) to
% <libRoot>/<lib>/exports/<icon>.svg, the file named by the library manifest.
% Regenerable; re-run after re-extracting the Modelica data.
%
% libRoot defaults to <nflow_blocks>/libraries.
%=============================================================================
function files = causalWriteIcons(libRoot)
  if nargin < 1 || isempty(libRoot)
    libRoot = [modulepath('nflow_blocks', 'root'), '/libraries'];
  end
  data = jsondecode(fileread([libRoot, '/msl-icons.json']));
  files = {};
  for k = 1:numel(data.blocks)
    if iscell(data.blocks)
      b = data.blocks{k};
    else
      b = data.blocks(k);
    end
    if ~isfield(b, 'lib') || isempty(b.lib)
      continue
    end
    icon = cw_icon_file(libRoot, b.lib, b.name);
    if isempty(icon)
      continue
    end
    svg = NFlow.internal.causalGenerateIcon(b.lib, b.name);
    p = [libRoot, '/', b.lib, '/exports/', icon];
    fid = fopen(p, 'w');
    if fid < 0
      error('Nelson:nflow:causalWriteIcons', 'Cannot write ''%s''.', p);
    end
    fwrite(fid, svg);
    fclose(fid);
    files{end + 1} = p; %#ok<AGROW>
  end
end
%=============================================================================
function icon = cw_icon_file(libRoot, lib, type)
  icon = '';
  p = [libRoot, '/', lib, '/library.json'];
  if ~isfile(p)
    return
  end
  manifest = jsondecode(fileread(p));
  for k = 1:numel(manifest.blocks)
    if iscell(manifest.blocks)
      b = manifest.blocks{k};
    else
      b = manifest.blocks(k);
    end
    if strcmp(b.type, type) && isfield(b, 'icon') && ~isempty(b.icon)
      icon = b.icon; return
    end
  end
end
%=============================================================================

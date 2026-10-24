%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% json = NFlow.internal.planarGenerateLibrary()
%
% Acausal palette N7/N5: the GUI palette manifest for the planar 2-D multibody
% blocks, generated from NFlow.internal.planarCatalog. Frames are the planar connectors
% (undirected, domain "planar"); they are exported as `physicalPins` so the GUI
% renders and wires them like other physical pins, and NFlow.internal.planarExpand turns
% the frame-wire diagram into a planarMechanicalIsland. Id "builtin.acausal_planar".
%=============================================================================
function json = planarGenerateLibrary()
  catalog = NFlow.internal.planarCatalog();
  blocks = {};
  for k = 1:numel(catalog)
    blocks{end + 1} = pl_gen_block(catalog(k)); %#ok<AGROW>
  end
  json = ['{', ...
    '"id":"builtin.acausal_planar",', ...
    '"title":"Planar (acausal)",', ...
    '"version":"1.0.0",', ...
    '"format":"nflow-2",', ...
    '"metadata":{"author":"Allan CORNET","tool":"Nelson nflow"},', ...
    '"comment":"Planar 2-D multibody blocks (frame connectors); lower to planarMechanicalIsland.",', ...
    '"license":"LGPL-3.0",', ...
    '"builtin":true,', ...
    '"blocks":[', strjoin(blocks, ','), ']}'];
end
%=============================================================================
function s = pl_gen_block(c)
  W = 90; H = 70;
  % Frame connectors -> physical pins (drop the '<named frames>' placeholder).
  frames = {};
  for f = 1:numel(c.frames)
    nm = c.frames{f};
    if ~isempty(strfind(nm, '<'))
      continue
    end
    frames{end + 1} = nm; %#ok<AGROW>
  end
  pins = {};
  nf = numel(frames);
  for f = 1:nf
    if nf >= 2
      if f == 1; px = 0; side = 'left'; py = round(H / 2);
      elseif f == 2; px = W; side = 'right'; py = round(H / 2);
      else; px = round(W / 2); side = 'top'; py = 0; end
    else
      px = 0; side = 'left'; py = round(H / 2);
    end
    pins{end + 1} = sprintf(...
      '{"name":"%s","x":%d,"y":%d,"side":"%s","domain":"planar"}', ...
      frames{f}, px, py, side); %#ok<AGROW>
  end
  % Sensors emit one signal output.
  outputs = '[]';
  if ~isempty(c.signal)
    outputs = sprintf('[{"x":%d,"y":%d,"side":"bottom","name":"out"}]', round(W / 2), H);
  end
  % Default params.
  dp = {};
  for qq = 1:numel(c.params)
    pm = c.params{qq};
    if isnumeric(pm{2})
      dp{end + 1} = sprintf('"%s":%.15g', pm{1}, pm{2}); %#ok<AGROW>
    else
      dp{end + 1} = sprintf('"%s":"%s"', pm{1}, pm{2}); %#ok<AGROW>
    end
  end
  defaultParams = ['{', strjoin(dp, ','), '}'];
  iconName = [c.name, '.svg'];
  s = sprintf(['{"type":"%s","label":"%s","acausal":true,"domain":"planar",', ...
    '"width":%d,"height":%d,"physicalPins":[%s],"inputs":[],"outputs":%s,', ...
    '"defaultParams":%s,"icon":"%s",', ...
    '"render":{"type":"image","src":"%s"},"help":"%s"}'], ...
    c.name, c.name, W, H, strjoin(pins, ','), outputs, ...
    defaultParams, iconName, iconName, pl_esc(c.desc));
end
%=============================================================================
function o = pl_esc(s)
  o = strrep(s, '\', '\\');
  o = strrep(o, '"', '\"');
end
%=============================================================================

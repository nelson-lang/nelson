%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% json = NFlow.internal.acausalGenerateLibrary(domain)
%
% Acausal palette N0.2: the manifest generator. Emits the GUI palette
% manifest (library.json content) for one acausal domain, generated entirely
% from the single-source catalog (NFlow.internal.acausalCatalog). This is the data
% pipeline the GUI palette (N5) consumes; it also DEFINES the physical-pin
% manifest format (`acausal`/`domain`/`physicalPins`), so the format is fixed
% here rather than being a prerequisite.
%
% domain: 'electrical' | 'thermal' | 'translational' | 'rotational'
% Returns the manifest as a JSON string. Writing it to
% <libraries>/acausal_<domain>/library.json makes the components palette-
% placeable once the GUI renders physical pins (N5); the SVG icons are added
% then (the manifest references them by path).
%
% Physical-pin format (defined here):
%   a component block is { "type":<catalog name>, "label", "acausal":true,
%     "domain":<domain>, "physicalPins":[ {"name","x","y","side","domain"} ],
%     "inputs":[...], "outputs":[...],   % causal ports for signal src/sensor
%     "defaultParams":{...}, "icon":"<type>.svg", "render":{...} }
% A physicalPin is undirected (no arrow) and typed by domain; a signal-driven
% source keeps a causal input, a sensor a causal output.
%=============================================================================
function json = acausalGenerateLibrary(domain)
  catalog = NFlow.internal.acausalCatalog();
  groupOf = containers.Map(...
    {'electrical', 'thermal', 'translational', 'rotational'}, ...
    {'Electrical', 'Thermal', 'Translational', 'Rotational'});
  if ~isKey(groupOf, domain)
    error('Nelson:nflow:acausalGenDomain', ...
      'Unknown acausal domain ''%s''.', domain);
  end
  group = groupOf(domain);

  blocks = {};
  for k = 1:numel(catalog)
    c = catalog(k);
    if ~strcmp(c.domain, domain)
      continue
    end
    blocks{end + 1} = gen_block(c); %#ok<AGROW>
  end

  json = ['{', ...
    '"id":"builtin.acausal_', domain, '",', ...
    '"title":"', group, ' (acausal)",', ...
    '"version":"1.0.0",', ...
    '"format":"nflow-2",', ...
    '"metadata":{"author":"Allan CORNET","tool":"Nelson nflow"},', ...
    '"comment":"Acausal ', lower(group), ' components (physical pins); lower to physicalIsland.",', ...
    '"license":"LGPL-3.0",', ...
    '"builtin":true,', ...
    '"blocks":[', strjoin(blocks, ','), ']}'];
end
%=============================================================================
function s = gen_block(c)
  W = 100; H = 80;
  % Physical pins: lay two-pin components left/right, one-pin on the left.
  pins = {};
  npin = numel(c.pins);
  for pp = 1:npin
    pinName = c.pins{pp}{1};
    pinDomain = c.domain;
    if numel(c.pins{pp}) >= 3
      pinDomain = c.pins{pp}{3};
    end
    if npin >= 2
      if pp == 1
        px = 0; side = 'left'; py = 40;
      elseif pp == 2
        px = W; side = 'right'; py = 40;
      else
        px = round(W / 2); side = 'top'; py = 0; % control pins (VCV/VCC)
      end
    else
      px = 0; side = 'left'; py = 40;
    end
    pins{end + 1} = sprintf(...
      '{"name":"%s","x":%d,"y":%d,"side":"%s","domain":"%s"}', ...
      pinName, px, py, side, pinDomain); %#ok<AGROW>
  end
  % Causal ports: a signal-driven source gets an input; a sensor an output.
  inputs = '[]';
  if ~isempty(c.signalIn)
    inputs = sprintf('[{"x":%d,"y":0,"side":"top","name":"%s"}]', round(W / 2), c.signalIn);
  end
  outputs = '[]';
  if ~isempty(c.signalOut)
    outputs = sprintf('[{"x":%d,"y":%d,"side":"bottom","name":"%s"}]', round(W / 2), H, c.signalOut);
  end
  % Default params from the catalog param map.
  dp = {};
  for qq = 1:numel(c.params)
    pm = c.params{qq};
    dp{end + 1} = sprintf('"%s":%.15g', pm{1}, pm{3}); %#ok<AGROW>
  end
  defaultParams = ['{', strjoin(dp, ','), '}'];
  % Icon file name matches NFlow.internal.acausalGenerateIcon / the catalog name.
  iconName = [c.name, '.svg'];
  s = sprintf(['{"type":"%s","label":"%s","acausal":true,"domain":"%s",', ...
    '"width":%d,"height":%d,"physicalPins":[%s],"inputs":%s,"outputs":%s,', ...
    '"defaultParams":%s,"icon":"%s",', ...
    '"render":{"type":"image","src":"%s"},"help":"%s"}'], ...
    c.name, c.name, c.domain, W, H, strjoin(pins, ','), inputs, outputs, ...
    defaultParams, iconName, iconName, esc(c.desc));
end
%=============================================================================
function o = esc(s)
  o = strrep(s, '"', '\"');
end
%=============================================================================

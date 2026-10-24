%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% count = NFlow.internal.paletteWriteHelp([blocksRoot])
%
% Acausal palette N5: materialise the per-block help pages for every acausal /
% planar palette in the CANONICAL nflow_blocks help format (same structure as
% the builtin block pages: keyword, short_description, syntax, param_input/
% output, a Module/Library/Type/Label table, the palette icon, implementation
% sources, see_also, history). Written to
%   help/{en_US,fr_FR}/xml/acausal_<domain>/<Name>.xml
% with a chapter.xml per group, reading the on-disk library.json manifests as
% the single source of truth. Regenerable; re-run after catalog/library edits.
%=============================================================================
function count = paletteWriteHelp(blocksRoot)
  if nargin < 1 || isempty(blocksRoot)
    blocksRoot = modulepath('nflow_blocks', 'root');
  end
  libRoot = [blocksRoot, '/libraries'];
  d = dir([libRoot, '/acausal_*']);
  langs = {'en_US', 'fr_FR'};
  count = 0;
  for i = 1:numel(d)
    if ~d(i).isdir
      continue
    end
    grp = d(i).name; % acausal_<domain>
    manifest = jsondecode(fileread([libRoot, '/', grp, '/library.json']));
    blocks = manifest.blocks;
    for L = 1:numel(langs)
      lang = langs{L};
      xmlDir = [blocksRoot, '/help/', lang, '/xml/', grp];
      if ~isdir(xmlDir)
        mkdir(xmlDir);
      end
      % chapter.xml for the group.
      chap = ph_chapter(manifest.title, grp, lang);
      fid = fopen([xmlDir, '/chapter.xml'], 'w'); fwrite(fid, chap); fclose(fid);
      % one page per block.
      for k = 1:numel(blocks)
        b = blocks(k);
        xml = ph_block_help(b, manifest, grp, lang, blocks);
        fid = fopen([xmlDir, '/', b.type, '.xml'], 'w');
        fwrite(fid, xml); fclose(fid);
        if L == 1
          count = count + 1;
        end
      end
    end
  end
end
%=============================================================================
function s = ph_chapter(title, grp, lang)
  desc = 'Acausal (physical) blocks with undirected pins, simulated natively through the differential-algebraic engine.';
  if strcmp(lang, 'fr_FR')
    desc = 'Blocs acausals (physiques) a broches non dirigees, simules nativement par le moteur differentiel-algebrique.';
  end
  s = ['<?xml version="1.0" encoding="UTF-8"?>', newline, ...
    '<xmldoc>', newline, ...
    '  <copyright>SAME AS NELSON SOFTWARE</copyright>', newline, ...
    '  <language>', lang, '</language>', newline, ...
    '  <chapter>', ph_esc(title), '</chapter>', newline, ...
    '  <chapter_description>', newline, '    <p>', desc, '</p>', newline, ...
    '  </chapter_description>', newline, '</xmldoc>', newline];
end
%=============================================================================
function s = ph_block_help(b, manifest, grp, lang, blocks)
  npin = 0;
  if isfield(b, 'physicalPins'); npin = numel(b.physicalPins); end
  nin = 0; if isfield(b, 'inputs'); nin = numel(b.inputs); end
  nout = 0; if isfield(b, 'outputs'); nout = numel(b.outputs); end
  desc = '';
  if isfield(b, 'help'); desc = b.help; end
  if isempty(desc); desc = b.label; end
  lead = desc;
  if strcmp(lang, 'fr_FR')
    lead = ['Composant acausal (', ph_esc(manifest.title), '). ', desc];
  end
  % see_also: up to 3 sibling blocks in the same group.
  sa = {};
  for k = 1:numel(blocks)
    if ~strcmp(blocks(k).type, b.type)
      sa{end + 1} = sprintf('    <see_also_item><link linkend="${nflow_blocks}%s/%s">%s</link></see_also_item>', ...
        grp, blocks(k).type, blocks(k).type); %#ok<AGROW>
    end
    if numel(sa) >= 3; break; end
  end
  island = 'physicalIsland';
  if strcmp(grp, 'acausal_planar'); island = 'planarMechanicalIsland';
  elseif any(strcmp(grp, {'acausal_translational', 'acausal_rotational'})); island = 'mechanicalTranslationalIsland'; end

  s = ['<?xml version="1.0" encoding="UTF-8"?>', newline, ...
    '<xmldoc>', newline, ...
    '  <copyright>SAME AS NELSON SOFTWARE</copyright>', newline, ...
    '  <language>', lang, '</language>', newline, ...
    '  <keyword>', b.type, '</keyword>', newline, ...
    '  <keyword_alias>block:', b.type, '</keyword_alias>', newline, ...
    '  <short_description>', ph_esc(desc), '</short_description>', newline, ...
    '  <syntax>', newline, '    <syntax_item>Block type: ', b.type, '</syntax_item>', newline, '  </syntax>', newline, ...
    '  <param_input>', newline, '    <param_input_item>', newline, ...
    '      <param_name>physical pins</param_name>', newline, ...
    '      <param_description>', num2str(npin), ' undirected physical pin(s); ', num2str(nin), ' signal input(s).</param_description>', newline, ...
    '    </param_input_item>', newline, '  </param_input>', newline, ...
    '  <param_output>', newline, '    <param_output_item>', newline, ...
    '      <param_name>signal ports</param_name>', newline, ...
    '      <param_description>', num2str(nout), ' signal output(s) (sensor readings).</param_description>', newline, ...
    '    </param_output_item>', newline, '  </param_output>', newline, ...
    '  <description>', newline, ...
    '    <p>', ph_esc(lead), '</p>', newline, ...
    '    <table border="1">', newline, '      <tbody>', newline, ...
    '        <tr><td>Module</td><td><code>nflow_blocks</code></td></tr>', newline, ...
    '        <tr><td>Library</td><td>', ph_esc(manifest.title), '</td></tr>', newline, ...
    '        <tr><td>Type</td><td><code>', b.type, '</code></td></tr>', newline, ...
    '        <tr><td>Label</td><td>', ph_esc(b.label), '</td></tr>', newline, ...
    '        <tr><td>Solver</td><td>Lowered to <code>', island, '</code>. Default solver <code>dae</code> (differential-algebraic); the fixed-step loop and the native explicit solvers (<code>ode1</code>/<code>ode4</code>/<code>ode45</code>) are also supported (a jointed multibody island requires <code>dae</code>).</td></tr>', newline, ...
    '      </tbody>', newline, '    </table>', newline, ...
    '    <p>', newline, '      <img src="../../../../libraries/', grp, '/exports/', b.icon, '" align="middle"/>', newline, '    </p>', newline, ...
    '    <p><b>Implementation Sources</b></p>', newline, ...
    '    <source_code label="Manifest" path="modules/nflow_blocks/libraries/', grp, '/library.json" language="json"/>', newline, ...
    ph_catalog_source(grp, b.type), ...
    '  </description>', newline, ...
    '  <used_function/>', newline, '  <bibliography/>', newline, ...
    '  <see_also>', newline, ph_join(sa), '  </see_also>', newline, ...
    '  <history>', newline, '    <history_item>', newline, ...
    '      <history_version>1.0.0</history_version>', newline, ...
    '      <history_description>initial version</history_description>', newline, ...
    '    </history_item>', newline, '  </history>', newline, ...
    '  <authors>', newline, '    <author_item>Allan CORNET</author_item>', newline, '  </authors>', newline, ...
    '</xmldoc>', newline];
end
%=============================================================================
function c = ph_catalog(grp)
  if strcmp(grp, 'acausal_planar'); c = '+NFlow/+internal/planarCatalog'; else; c = '+NFlow/+internal/acausalCatalog'; end
end
function s = ph_catalog_source(grp, type)
  % Embeds only this block's catalog entry. A block without a catalog entry
  % (declared in the manifest alone) gets no Catalog source.
  rel = ['modules/nflow_blocks/functions/', ph_catalog(grp), '.m'];
  catalog = fileread([nelsonroot(), '/', rel]);
  if isempty(strfind(catalog, ['entry(''', type, '''']))
    s = '';
    return
  end
  s = ['    <source_code label="Catalog" path="', rel, '" language="matlab" start="entry(''', type, '''" end="'');"/>', newline];
end
function s = ph_join(items)
  if isempty(items); s = ''; return; end
  s = [strjoin(items, [newline]), newline];
end
function o = ph_esc(s)
  o = strrep(s, '&', '&amp;');
  o = strrep(o, '<', '&lt;');
  o = strrep(o, '>', '&gt;');
end
%=============================================================================

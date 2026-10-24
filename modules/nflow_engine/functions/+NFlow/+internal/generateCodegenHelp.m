%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nChanged = NFlow.internal.generateCodegenHelp([helpRoot])
%
% Stamp each block help page with a code-generation indicator in its
% "Extended Capabilities" section, derived from the authoritative registry via
% __nflow_codegen_support__(type). Blocks whose type the C and/or Rust generator
% can lower get a line saying so; blocks with no code-generation support get no
% line (and any previously stamped line is removed). Idempotent -- safe to re-run
% whenever block codegen support changes.
%
% helpRoot defaults to <nflow_blocks module>/help. Both en_US and fr_FR pages
% under helpRoot/<lang>/xml/<category>/*.xml are processed (chapter.xml skipped),
% each with its own labels. Pages without the "Implementation Sources" anchor
% (the simpler acausal/planar layout) are left untouched -- those blocks have no
% code generation anyway. All help pages are ASCII, so bytes are preserved.
%=============================================================================
function nChanged = generateCodegenHelp(helpRoot)
  if nargin < 1 || isempty(helpRoot)
    helpRoot = [modulepath('nflow_blocks'), '/help'];
  end
  nChanged = 0;
  langs = {'en_US', 'fr_FR'};
  for li = 1:numel(langs)
    lang = langs{li};
    if strcmp(lang, 'fr_FR')
      anchor = '<b>Sources d implementation</b>';
      marker = '<p>Generation de code';
      prefix = 'Generation de code : prise en charge pour ';
      sBoth = 'C et Rust';
    else
      anchor = '<b>Implementation Sources</b>';
      marker = '<p>Code generation:';
      prefix = 'Code generation: supported for ';
      sBoth = 'C and Rust';
    end
    xmlRoot = [helpRoot, '/', lang, '/xml'];
    if ~isdir(xmlRoot)
      continue;
    end
    cats = dir(xmlRoot);
    for ci = 1:numel(cats)
      nm = cats(ci).name;
      if ~cats(ci).isdir || nm(1) == '.'
        continue;
      end
      catDir = [xmlRoot, '/', nm];
      files = dir([catDir, '/*.xml']);
      for fi = 1:numel(files)
        fn = files(fi).name;
        if strcmp(fn, 'chapter.xml')
          continue;
        end
        f = [catDir, '/', fn];
        fid = fopen(f, 'rb');
        bytes = fread(fid, inf, 'uint8')';
        fclose(fid);
        raw = char(bytes);
        % The syntax line is "Block type: <type>" (en) or, on some fr pages, the
        % translated "Type de bloc : <type>". The type value itself is identical.
        tok = regexp(raw, '(?:Block type|Type de bloc)\s*:\s*([A-Za-z0-9_]+)', 'tokens', 'once');
        if isempty(tok)
          continue;
        end
        type = tok{1};
        [cOk, rustOk] = __nflow_codegen_support__(type);
        support = '';
        if cOk && rustOk
          support = sBoth;
        elseif cOk
          support = 'C';
        elseif rustOk
          support = 'Rust';
        end
        NL = char(10);
        if ~isempty(strfind(raw, char(13)))
          NL = [char(13), char(10)];
        end
        nl = length(NL);
        original = raw;
        % Remove any previously stamped paragraph (idempotency).
        cg = strfind(raw, marker);
        if ~isempty(cg)
          cgstart = cg(1);
          lineStart = 1;
          pre = strfind(raw(1:cgstart - 1), NL);
          if ~isempty(pre)
            lineStart = pre(end) + nl;
          end
          et = strfind(raw(cgstart:end), '</p>');
          ae = cgstart + et(1) + 3;
          removeUpto = numel(raw);
          na = strfind(raw(ae:end), NL);
          if ~isempty(na)
            removeUpto = ae + na(1) + nl - 2;
          end
          raw = [raw(1:lineStart - 1), raw(removeUpto + 1:end)];
        end
        % Insert the current indicator. Preferred anchor: the "Implementation
        % Sources" header paragraph (in the Extended Capabilities section).
        % Fallback for the simpler layout (no such header, e.g. toWorkspace):
        % the first <source_code> tag. anchorStart = 0 means no place found.
        if ~isempty(support)
          anchorStart = 0;
          ip = strfind(raw, anchor);
          if ~isempty(ip)
            po = strfind(raw(1:ip(1)), '<p>');
            if ~isempty(po)
              anchorStart = 1;
              pre2 = strfind(raw(1:po(end) - 1), NL);
              if ~isempty(pre2)
                anchorStart = pre2(end) + nl;
              end
            end
          else
            sc = strfind(raw, '<source_code');
            if ~isempty(sc)
              anchorStart = 1;
              pre2 = strfind(raw(1:sc(1) - 1), NL);
              if ~isempty(pre2)
                anchorStart = pre2(end) + nl;
              end
            end
          end
          if anchorStart > 0
            para = ['    <p>', prefix, support, '.</p>', NL];
            raw = [raw(1:anchorStart - 1), para, raw(anchorStart:end)];
          end
        end
        if ~strcmp(raw, original)
          fid = fopen(f, 'wb');
          fwrite(fid, double(raw), 'uint8');
          fclose(fid);
          nChanged = nChanged + 1;
        end
      end
    end
  end
end
%=============================================================================

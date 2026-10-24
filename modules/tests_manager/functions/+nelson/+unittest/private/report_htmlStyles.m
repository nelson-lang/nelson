%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function lines = report_htmlStyles()
  lines = {
    '<style>'
    ':root{color-scheme:light dark;--bg:#f7f7f4;--fg:#202124;--muted:#666;--line:#d8d8d0}'
    ':root{--panel:#fff;--pass:#147a3d;--fail:#b42318;--skip:#7a5c00;--bench:#1d4ed8}'
    '@media(prefers-color-scheme:dark){:root{--bg:#151515;--fg:#ececec;--muted:#aaa;--line:#3a3a3a;--panel:#202020}}'
    'body{margin:0;background:var(--bg);color:var(--fg);font:14px/1.45 system-ui,-apple-system,Segoe UI,sans-serif}'
    'main{max-width:1180px;margin:0 auto;padding:24px}'
    'header{display:flex;gap:16px;align-items:end;justify-content:space-between;margin-bottom:18px}'
    'h1{font-size:28px;margin:0}h2{font-size:13px;margin:0 0 6px;color:var(--muted);text-transform:uppercase}'
    '.config-strip{display:flex;gap:8px;flex-wrap:wrap;margin:0 0 14px}'
    '.config-strip span{background:var(--panel);border:1px solid var(--line);border-radius:6px;padding:5px 8px}'
    '.summary{display:grid;grid-template-columns:repeat(auto-fit,minmax(120px,1fr));gap:10px;margin:18px 0}'
    '.run-info{background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:12px}'
    '.info-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:10px 18px}'
    '.info-grid div{min-width:0}.info-grid dt{color:var(--muted);font-size:12px}'
    '.info-grid dd{margin:2px 0 0;word-break:break-word}'
    '.metric{background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:10px}'
    '.metric span{display:block;color:var(--muted);font-size:12px}.metric strong{font-size:20px}'
    '.overview-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(360px,1fr));gap:12px;margin:16px 0}'
    '.overview-panel{min-width:0;background:var(--panel);border:1px solid var(--line);border-radius:8px;padding:12px}'
    '.compact-table{font-size:13px}.compact-table th,.compact-table td{padding:6px}'
    '.toolbar{display:flex;gap:10px;flex-wrap:wrap;margin:12px 0 16px}'
    '.toolbar input,.toolbar select,.toolbar button{padding:8px;border:1px solid var(--line);border-radius:6px}'
    '.toolbar input,.toolbar select,.toolbar button{background:var(--panel);color:var(--fg)}'
    '.toolbar button.active{outline:2px solid var(--bench)}'
    'table{width:100%;border-collapse:collapse;background:var(--panel);border:1px solid var(--line)}'
    'th,td{padding:8px;border-bottom:1px solid var(--line);text-align:left;vertical-align:top}'
    'th{position:sticky;top:0;background:var(--panel);z-index:1}.num{text-align:right}'
    'th[data-sort]{cursor:pointer}th[data-direction="asc"]::after{content:" ↑"}'
    'th[data-direction="desc"]::after{content:" ↓"}'
    '.module-row td{background:rgba(127,127,127,.10);font-weight:700;color:var(--muted)}'
    '.badge{display:inline-block;border-radius:999px;padding:2px 8px;color:white;font-size:12px}'
    '.badge.passed{background:var(--pass)}.badge.failed,.badge.timeout{background:var(--fail)}'
    '.badge.skipped{background:var(--skip)}.badge.bench{background:var(--bench)}'
    '.badge.filtered{background:var(--muted)}.details-row td{padding:0 8px 8px}'
    '.details-toggle{width:22px;height:22px;margin:0 6px 0 0;border:1px solid var(--line)}'
    '.details-toggle{border-radius:4px;background:var(--panel);color:var(--fg);cursor:pointer}'
    '.case-index{color:var(--muted)}.details-grid{display:grid;gap:10px;margin:8px 0 12px}'
    'pre{white-space:pre-wrap;overflow:auto;background:rgba(127,127,127,.12);padding:10px}'
    'pre{border-radius:6px;color:var(--fg)}'
    '.hidden{display:none}.meta{color:var(--muted)}'
    '</style>'
  };
  lines = lines(:);
end
%=============================================================================

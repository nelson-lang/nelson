%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% markdown_to_typst converts the hand-written pages of the manual (home page,
% changelogs, licenses) used by buildhelptypst. It is a private helper of
% help_tools: the test runs from its directory to reach it.
%=============================================================================
previous_dir = pwd();
cleanup = onCleanup(@() cd(previous_dir));
cd([modulepath('help_tools'), '/functions/private']);
%=============================================================================
md = {'<link rel="stylesheet" href="x.css">';
'<img src="banner.png" alt="Nelson">';
'# Nelson 2.0.0';
'';
'**Nelson 2.0** is a *major* release with `classdef` and 50% more #tags.';
'See the [Changelog](CHANGELOG.md) and <https://nelson-lang.github.io/>.';
'';
'## Features';
'';
'- faster engine';
'  - nested item';
'1. first step';
'2. second step';
'';
'```matlab';
'x = [1, 2]; % #not escaped';
'```';
'';
'| Name | Value |';
'| --- | --- |';
'| alpha | 1 |';
'| beta | 2 |';
''};
typst = markdown_to_typst(strjoin(md, char(10)), 'main:homepage', '../nelson_help.typ');
%=============================================================================
asserts.istrue(startsWith(typst, ['#import "../nelson_help.typ": *', newline]));
% html-only tags dropped, images centered
asserts.isfalse(contains(typst, 'stylesheet'));
asserts.istrue(contains(typst, '#align(center)[#image("banner.png")]'));
% first heading carries the page label, following headings keep their depth
asserts.istrue(contains(typst, ['= Nelson 2.0.0 <main:homepage>', newline]));
asserts.istrue(contains(typst, ['== Features', newline]));
% inline constructs and escaping
asserts.istrue(contains(typst, '#strong[Nelson 2.0]; is a #emph[major]; release with #raw("classdef"); and 50% more \#tags.'));
asserts.istrue(contains(typst, 'See the #nlink(<main:CHANGELOG>)[Changelog]; and #link("https://nelson-lang.github.io/");.'));
% lists keep nesting and marker type
asserts.istrue(contains(typst, [newline, '- faster engine', newline, '  - nested item', newline]));
asserts.istrue(contains(typst, [newline, '+ first step', newline, '+ second step', newline]));
% fenced code: six backticks, language identifier mapped, content verbatim
asserts.istrue(contains(typst, ['``````matlab', newline, 'x = [1, 2]; % #not escaped', newline, '``````']));
% pipe table: header row, separator dropped
asserts.istrue(contains(typst, ['#table(', newline, '  columns: 2,', newline, '  table.header([Name], [Value]),', newline, '  [alpha], [1],', newline, '  [beta], [2],', newline, ')']));
%=============================================================================
% default helpers path and a page whose first heading is not level 1
typst = markdown_to_typst(['### Deep title', newline, 'text', newline, '#### Sub'], 'main:deep');
asserts.istrue(startsWith(typst, ['#import "nelson_help.typ": *', newline]));
asserts.istrue(contains(typst, ['= Deep title <main:deep>', newline]));
asserts.istrue(contains(typst, ['== Sub', newline]));
%=============================================================================
clear cleanup;
asserts.isequal(pwd(), previous_dir);
%=============================================================================

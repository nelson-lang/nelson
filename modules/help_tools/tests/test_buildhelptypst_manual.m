%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--TIMEOUT 300-->
% buildhelptypst(dest) builds the whole manual: home page, getting started,
% every module, changelogs and licenses, assembled by <dest>/<lang>/main.typ.
%=============================================================================
have_xmldoc = isfile([modulepath('help_tools'), '/help/en_US/xml/chapter.xml']);
skip_testsuite(~have_xmldoc, 'Help XML sources are not available in this build')
have_md = isfile([nelsonroot(), '/modules/main/help/en_US/md/homepage.md']);
skip_testsuite(~have_md, 'Markdown front matter is missing')
%=============================================================================
destination = tempname();
mkdir(destination);
buildhelptypst(destination);
en = [destination, '/en'];
asserts.istrue(isfile([en, '/main.typ']));
asserts.istrue(isfile([en, '/nelson_help.typ']));
asserts.istrue(isfile([en, '/homepage.typ']));
asserts.istrue(isfile([en, '/getting_started.typ']));
asserts.istrue(isfile([en, '/banner_homepage.png']));
asserts.istrue(isfile([en, '/changelogs/CHANGELOG.typ']));
asserts.istrue(isfile([en, '/license/license.typ']));
asserts.istrue(isfile([en, '/license/lgpl-3.0.typ']));
asserts.istrue(isfile([en, '/license/gpl-3.0.typ']));
asserts.istrue(isfile([en, '/help_tools/main.typ']));
asserts.istrue(isfile([en, '/help_tools/buildhelptypst.typ']));
asserts.istrue(isfile([destination, '/fr/main.typ']));
%=============================================================================
content = fileread([en, '/main.typ']);
asserts.istrue(startsWith(content, ['#import "nelson_help.typ": nelson-style', newline, '#set document(title: "Nelson")', newline, '#show: nelson-style.with(lang: "en")']));
% order: front matter, modules, changelogs, licenses, outline last
home = strfind(content, '#include "homepage.typ"');
started = strfind(content, '#include "getting_started.typ"');
module = strfind(content, '#include "help_tools/main.typ"');
changelog = strfind(content, '#include "changelogs/CHANGELOG.typ"');
license = strfind(content, '#include "license/gpl-3.0.typ"');
outline = strfind(content, '#outline(depth: 2)');
asserts.istrue(~isempty(home) && ~isempty(started) && ~isempty(module) && ~isempty(changelog) && ~isempty(license) && ~isempty(outline));
asserts.istrue(home < started && started < module && module < changelog && changelog < license && license < outline);
%=============================================================================
% front matter pages: label for cross-references, helpers import
content = fileread([en, '/homepage.typ']);
asserts.istrue(startsWith(content, ['#import "nelson_help.typ": *', newline]));
asserts.istrue(contains(content, '<main:homepage>'));
content = fileread([en, '/changelogs/CHANGELOG.typ']);
asserts.istrue(startsWith(content, ['#import "../nelson_help.typ": *', newline]));
asserts.istrue(contains(content, '<main:CHANGELOG>'));
%=============================================================================
rmdir(destination, 's');
%=============================================================================

%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/101
% <-- Short Description -->
% Allows cell_vertcat_generic & cell_horzcat_generic
%=============================================================================
% VERTCAT
%=============================================================================
content = {'# Languages'; ''};
r = [content; 'r'];
ref = {'# Languages'; ''; 'r'};
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages'; ''};
r = ['r'; content];
ref = {'r'; '# Languages'; ''};
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages'; ''};
r = [[]; content];
ref = content;
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages'; ''};
r = [{}; content];
ref = content;
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages'; ''};
r = [1; content];
ref = {1; '# Languages'; ''};
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages'; ''};
r = [content; 1];
ref = {'# Languages'; ''; 1};
asserts.isequal(r, ref);
%=============================================================================
asserts.checkerror('r = [content;cell(3,3)];', _('Dimensions of arrays being concatenated are not consistent.'))
%=============================================================================
% HORZCAT
%=============================================================================
content = {'# Languages', ''};
r = [content, 'r'];
ref = {'# Languages', '', 'r'};
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages', ''};
r = ['r', content];
ref = {'r', '# Languages', ''};
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages', ''};
r = [content, []];
ref = {'# Languages', ''};
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages', ''};
r = [[], content];
ref = content;
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages', ''};
r = [content, {}];
ref = {'# Languages', ''};
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages', ''};
r = [{}, content];
ref = content;
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages', ''};
r = [content, 1];
ref = {'# Languages', '', 1};
asserts.isequal(r, ref);
%=============================================================================
content = {'# Languages', ''};
r = [1, content];
ref = {1, '# Languages', ''};
asserts.isequal(r, ref);
%=============================================================================
asserts.checkerror('r = [content,cell(3,3)];', _('Dimensions of arrays being concatenated are not consistent.'))
%=============================================================================
r = ['aal', [{'ffffff'}, 'ffffffffffff']];
ref = {'aal', 'ffffff', 'ffffffffffff'};
asserts.isequal(r, ref);
%=============================================================================
r = {};
r = [r, 'r1'];
r = [r, 'r2'];
r = [r, 'r3'];
REF = {'r1', 'r2', 'r3'};
asserts.isequal(r, REF);
%=============================================================================
r = {};
r = [r; 'r1'];
r = [r; 'r2'];
r = [r; 'r3'];
REF = {'r1'; 'r2'; 'r3'};
asserts.isequal(r, REF);
%=============================================================================

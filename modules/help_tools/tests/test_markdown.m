%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = markdown('### Hello');
REF = '<h3>Hello</h3>
';
asserts.isequal(R, REF);
%=============================================================================
R = markdown('**Hello**');
REF = '<p><strong>Hello</strong></p>
';
asserts.isequal(R, REF);
%=============================================================================
md_path = [modulepath('help_tools', 'root'), '/tests/md/readme.md'];
asserts.istrue(isfile(md_path));
%=============================================================================
dest_path = [tempname(), '.html'];
asserts.istrue(markdown(md_path, dest_path));
R = fileread(dest_path);
REF =  '<h3>Hello</h3>
<p><strong>World</strong></p>
';
asserts.isequal(R, REF);
rmfile(dest_path);
%=============================================================================
txt = 'Hello <script>alert("XSS")</script> World';
advanced_html = markdown(txt, 'advanced');
REF = '<p>Hello <script>alert(“XSS”)</script> World</p>
';
asserts.isequal(advanced_html, REF);
%=============================================================================
secure_html = markdown(txt, 'secure');
REF = '<p>Hello <!-- raw HTML omitted -->alert(&quot;XSS&quot;)<!-- raw HTML omitted --> World</p>
';
asserts.isequal(secure_html, REF);
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
probePath = getenv('NELSON_QT_WORKSPACE_PROBE');
skip_testsuite(isempty(probePath) || ~isfile(probePath), 'Build the Qt workspace probe and set NELSON_QT_WORKSPACE_PROBE.');
close all;
surf(peaks);
figureObject = gcf();
drawnow;
asserts.istrue(~isempty(__figure_native_geometry__(figureObject)));
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
figureObject.Name = 'Qt desktop visibility';
figureObject.NumberTitle = 'off';
drawnow;
request = jsonencode(struct('operation', 'presented'));
asserts.isequal(dlcall(probe, figureObject.Name, request), int32(1));
frame = getframe(figureObject);
asserts.istrue(~isempty(frame.cdata));
asserts.istrue(max(frame.cdata(:)) > min(frame.cdata(:)));
figureObject.WindowStyle = 'docked';
drawnow;
asserts.isequal(dlcall(probe, figureObject.Name, request), int32(1));
figureObject.WindowStyle = 'normal';
drawnow;
asserts.isequal(dlcall(probe, figureObject.Name, request), int32(1));
figureObject.Visible = 'off';
drawnow;
asserts.isequal(dlcall(probe, figureObject.Name, request), int32(0));
figureObject.Visible = 'on';
drawnow;
asserts.isequal(dlcall(probe, figureObject.Name, request), int32(1));
close(figureObject);

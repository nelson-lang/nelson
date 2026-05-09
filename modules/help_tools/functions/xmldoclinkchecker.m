%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xmldoclinkchecker(varargin)
    narginchk(0, 1);
    nargoutchk(0, 3);

    if nargin == 0
        modules_path = [nelsonroot(), '/modules/'];
        module_list = dir(modules_path);
        disp(_('Checking XML documentation links...'));
        for i = 1:length(module_list)
            if module_list(i).isdir && ~ismember(module_list(i).name, {'.', '..'})
                module_name = module_list(i).name;
                for lang = getavailablelanguages()'
                    help_path = [modules_path, module_name, '/help/', lang{1}, '/xml/'];
                    if isdir(help_path) && isfile([help_path, 'chapter.xml'])
                        switch nargout()
                        case 0
                            xmldoclinkchecker(help_path);
                        case 1
                            state = xmldoclinkchecker(help_path);
                            if ~state
                                varargout{1} = state;
                                return
                            end
                        case 2
                            [state, errors_detected] = xmldoclinkchecker(help_path);
                            if ~state
                                varargout{1} = state;
                                varargout{2} = errors_detected;
                                return
                            end
                        case 3
                            [state, errors_detected, warnings_detected] = xmldoclinkchecker(help_path);
                            if ~state
                                varargout{1} = state;
                                varargout{2} = errors_detected;
                                varargout{3} = warnings_detected;
                                return
                            end
                        end
                    end
                end
            end
        end
        switch nargout()
            case 0
                varargout = {};
            case 1
                varargout{1} = true;
            case 2
                varargout{1} = true;
                varargout{2} = {};
            case 3
                varargout{1} = true;
                varargout{2} = {};
                varargout{3} = {};
            otherwise
                varargout = {};
        end
        return;
    else
        target = varargin{1};
        if isdir(target)
            xmlfiles = dir([target, '/*.xml'], '-s');
            files_to_check = cell(1, length(xmlfiles));
            for j = 1:length(xmlfiles)
                files_to_check{j} = [xmlfiles(j).folder, '/', xmlfiles(j).name];
            end
            index_dirs = localResolveIndexDirectories(target);
        else
            if ~isfile(target)
                error(_('File not found: %s'), target);
            end
            files_to_check = {target};
            index_dirs = localResolveIndexDirectories(fileparts(target));
        end
    end

    [state, errors_detected, warnings_detected] = __xmldoclinkchecker__(index_dirs, files_to_check);

    if nargout == 0
        if ~state
            error(errors_detected{1});
        end
        return;
    end

    switch nargout()
    case 1
        varargout = {state};
    case 2
        varargout = {state, errors_detected};
    case 3
        varargout = {state, errors_detected, warnings_detected};
    otherwise
        varargout = {};
    end
end

%=============================================================================
function index_dirs = localGetAllModuleXmlDirectories(language)
    if nargin < 1
        language = '';
    end
    modules_path = [nelsonroot(), '/modules/'];
    module_list = dir(modules_path);
    index_dirs = {};
    for i = 1:length(module_list)
        if module_list(i).isdir && ~ismember(module_list(i).name, {'.', '..'})
            module_name = module_list(i).name;
            if isempty(language)
                languages = getavailablelanguages();
            else
                languages = {language};
            end
            for lang = languages(:)'
                help_path = [modules_path, module_name, '/help/', lang{1}, '/xml'];
                if isdir(help_path) && isfile([help_path, '/chapter.xml'])
                    index_dirs{end + 1} = help_path; %#ok<AGROW>
                end
            end
        end
    end
end

%=============================================================================
function index_dirs = localResolveIndexDirectories(pathname)
    normalized = strrep(pathname, '\', '/');
    modules_index = strfind(normalized, '/modules/');
    if ~isempty(modules_index)
        help_index = strfind(normalized, '/help/');
        xml_index = strfind(normalized, '/xml');
        if ~isempty(help_index) && ~isempty(xml_index)
            help_index = help_index(length(help_index));
            xml_index = xml_index(length(xml_index));
            if xml_index > help_index
                language = normalized(help_index + length('/help/'):xml_index - 1);
                if ~isempty(language) && isempty(strfind(language, '/'))
                    index_dirs = localGetAllModuleXmlDirectories(language);
                    return;
                end
            end
        end
    end

    xml_root = localFindXmlRoot(pathname);
    if isempty(xml_root)
        error(_('No XML documentation root found from: %s'), pathname);
    end
    index_dirs = {xml_root};
end

%=============================================================================
function xml_root = localFindXmlRoot(pathname)
    xml_root = '';
    current = pathname;
    while ~isempty(current)
        if isfile([current, '/chapter.xml'])
            xml_root = current;
        end
        parent = fileparts(current);
        if strcmp(parent, current)
            break;
        end
        current = parent;
    end
end
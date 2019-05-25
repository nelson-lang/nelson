#==============================================================================
# Copyright (c) 2016-present Allan CORNET (Nelson)
#==============================================================================
# This file is part of the Nelson.
#=============================================================================
# LICENCE_BLOCK_BEGIN
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# Alternatively, you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program. If not, see <http://www.gnu.org/licenses/>.
# LICENCE_BLOCK_END
#==============================================================================

FROM archlinux/base:latest
MAINTAINER Allan CORNET "nelson.numerical.computation@gmail.com"

ARG BRANCH_NAME
RUN echo "Nelson's branch: ${BRANCH_NAME}"

RUN pacman -Syu --noconfirm
RUN pacman -S base-devel --noconfirm
RUN pacman -S inetutils --noconfirm
RUN pacman -S gawk --noconfirm
RUN pacman -S m4 --noconfirm
RUN pacman -S pkg-config --noconfirm
RUN pacman -S boost-libs boost --noconfirm
RUN pacman -S cmake --noconfirm
RUN pacman -S libffi --noconfirm
RUN pacman -S icu --noconfirm
RUN pacman -S qt5-base --noconfirm
RUN pacman -S qt5-tools --noconfirm
RUN pacman -S qt5-webkit --noconfirm
RUN pacman -S libxml2 --noconfirm
RUN pacman -S gcc --noconfirm
RUN pacman -S make --noconfirm
RUN pacman -S blas --noconfirm
RUN pacman -S lapack --noconfirm
RUN pacman -S lapacke --noconfirm
RUN pacman -S fftw --noconfirm
RUN pacman -S openmpi --noconfirm
RUN pacman -S hdf5 --noconfirm
RUN pacman -S libmatio --noconfirm
RUN pacman -S taglib --noconfirm
RUN pacman -S portaudio --noconfirm
RUN pacman -S libsndfile --noconfirm
RUN pacman -S git --noconfirm


RUN git clone https://github.com/live-clones/hdf5.git /tmp/hdf5_1_10_5 
RUN cd /tmp/hdf5_1_10_5 && git checkout hdf5-1_10_5 && ./configure --quiet --enable-shared --disable-deprecated-symbols --disable-hl --disable-strict-format-checks --disable-memory-alloc-sanity-check --disable-instrument --disable-parallel --disable-trace --disable-asserts --with-pic --with-default-api-version=v110 CFLAGS="-w" && make install -C src

RUN git clone https://github.com/tbeu/matio /tmp/matio 
RUN cd /tmp/matio && git checkout v1.5.15 && cd /tmp/matio && ./autogen.sh && ./configure --enable-shared --enable-mat73=yes --enable-extended-sparse=no --with-pic --with-hdf5=/tmp/hdf5_1_10_5/hdf5 && make && make install;

RUN git clone https://github.com/eigenteam/eigen-git-mirror /tmp/eigen
RUN mkdir /tmp/eigen-build && cd /tmp/eigen && git checkout 3.3.7 && cd - && cd /tmp/eigen-build && cmake . /tmp/eigen && make -j4 && make install;

RUN git clone https://github.com/Nelson-numerical-software/nelson.git
WORKDIR "/nelson"
RUN cd "/nelson" && git checkout ${BRANCH_NAME}

RUN mkdir /home/nelsonuser

RUN groupadd -g 1001 nelsonuser && \
    useradd -r -u 1001 -g nelsonuser nelsonuser

RUN chown -R nelsonuser:nelsonuser /home/nelsonuser

RUN chown -R nelsonuser:nelsonuser /nelson

USER nelsonuser

ENV AUDIODEV null

RUN cmake -G "Unix Makefiles" .
RUN make -j4

RUN make buildhelp
RUN make tests_minimal
RUN make package
RUN make tests_all_no_display
    

ENTRYPOINT ["/nelson/bin/linux64/nelson-cli"]

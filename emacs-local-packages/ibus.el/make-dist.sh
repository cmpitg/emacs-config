#!/bin/bash
# Copyright (c) 2010 and onwards, S. Irie
# This program is distributed under the MIT Licence.

EL_DEV="ibus-dev.el"
EL_MAIN="ibus.el"
DBGREGEXP="^;*\t* *(ibus-log\b"

FILES=("ibus-el-agent" "README" "doc" "debian")

PACKAGE="ibus-el"
VERSION=$(sed -n 's/^(defconst ibus-mode-version "\(.*\)")$/\1/p' $EL_DEV)


# Command line options =================

DCH="no"
DEBUILD="no"
BLDTYPE="deb"
NATIVE="yes"
BLDFLAGS=""
SIGN="no"

while getopts :hcbSpnus OPTION
do
    case $OPTION in
	h)
	    echo "usage: $0 [OPTION]..."
	    echo "  -c  edit debian/changelog"
	    echo "  -b  build deb package"
	    echo "  -S  build source package"
	    echo "  -p  build deb package using pbuilder"
	    echo "  -n  build deb package as a non-native one"
	    echo "  -u  add -us -uc options to debuild"
	    echo "  -s  create a digital signature"
	    echo "  -h  display this help and exit"
	    exit
	    ;;
	c)
	    DCH="yes"
	    ;;
	b)
	    DEBUILD="yes"
	    ;;
	S)
	    if [ "$BLDTYPE" == "pbuild" ]; then
		echo "$0: options -p and -S can't be used together." >&2
		exit 1
	    fi

	    DEBUILD="yes"
	    BLDTYPE="src"
	    ;;
	p)
	    if [ "$BLDTYPE" == "src" ]; then
		echo "$0: options -S and -p can't be used together." >&2
		exit 1
	    fi

	    DEBUILD="yes"
	    BLDTYPE="pbuild"
	    ;;
	n)
	    NATIVE="no"
	    ;;
	u)
	    BLDFLAGS="${BLDFLAGS} -us -uc"
	    ;;
	s)
	    SIGN="yes"
	    ;;
	?)
	    echo "$0: invalid option -- $OPTARG" >&2
	    echo "Try \`$0 -h' for more information." >&2
	    exit 1
	    ;;
    esac
done


# Paths ================================

WORKDIR="v${VERSION}"
ARCHDIR="${WORKDIR}/${PACKAGE}-${VERSION}"
ARCHFILE="${ARCHDIR}.tar.gz"
ARDEBDIR="${ARCHDIR}/debian"
ORIGARCH="${WORKDIR}/${PACKAGE}_${VERSION}.orig.tar.gz"
CURRDIR="$PWD"


# Locate files =========================

rm -rfv $WORKDIR
mkdir -pv $ARCHDIR

if [ "$DBGREGEXP" == "" ]; then
    cp -pv $EL_DEV ${ARCHDIR}/${EL_MAIN}
else
    echo "comment out debug codes in $EL_DEV -> ${ARCHDIR}/${EL_MAIN}"
    sed "s/\(${DBGREGEXP}\)/;#\1/" $EL_DEV > ${ARCHDIR}/${EL_MAIN}
fi

for i in ${FILES[@]}; do
    cp -rpv $i $ARCHDIR
done

for i in $(find $ARCHDIR -regex '.*\(~\|\.~[0-9]+~\)'); do
    rm -fv $i      # Delete backup files
done


# Edit changelog =======================

if [ "$DCH" == "yes" ]; then
    cd $ARDEBDIR
    dch -v ${VERSION}
    cd $CURRDIR

    cp -pv --backup=t ${ARDEBDIR}/changelog debian/changelog
fi


# Create archive =======================

echo "create archive $ARCHFILE"
cd $WORKDIR
tar cvzf $(basename $ARCHFILE) $(basename $ARCHDIR)
cd $CURRDIR


# Create a digital signature ===========

if [ "$SIGN" == "yes" ]; then
    cd $WORKDIR
    gpg --armor --sign --detach-sig $(basename $ARCHFILE)
    cd $CURRDIR
fi


# Build deb package  ===================

if [ ! "$DEBUILD" == "yes" ]; then
    exit 0
fi

if [ "$NATIVE" == "no" ]; then
    cp -pv $ARCHFILE $ORIGARCH
fi

cd $ARCHDIR

case $BLDTYPE in
    deb)
	debuild ${BLDFLAGS}
	;;
    src)
	debuild -S ${BLDFLAGS}
	;;
    pbuild)
	debuild -S ${BLDFLAGS} && sudo pbuilder build ../*.dsc
	;;
esac

cd $CURRDIR

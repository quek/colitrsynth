# memo

- CL-MeltySynth
  https://chiselapp.com/user/MistressRemilia/repository/CL-MeltySynth/dir?ci=tip
- Sounds Like Common Lisp - ゼロからはじめるサウンドプログラミング
  https://www.slideshare.net/t-sin/sounds-like-common-lisp
  https://github.com/t-sin/pukunui

# セットアップ

ref. https://pkulev.itch.io/o2/devlog/79386/the-windows-build-how-to-build-lisp-programs-that-require-libffi-on-windows

- [MSYS2](https://www.msys2.org) をインストール
- `pacman -S gcc pkg-config libffi-devel mingw-w64-x86_64-{SDL2,SDL2_image,SDL2_ttf}`
- 「環境変数を編集」で PATH に C:\msys64\usr\bin を追加

## sdl2-ttf

sdl2-ttf:open-font で make-ttf-font がないと言われる件
c:/Users/ancient/quicklisp/dists/quicklisp/software/cl-sdl2-ttf-20200925-git/sdl2-ttf-asd
の `depends-on` の `:cl-autowrap` を `:cl-autowrap/libffi` に変える。

## DLL

TODO 要整理

dll ディレクトリに必要なのおいている。

~/.sbclrc

```
(ql:quickload :cffi)
(setf cffi:*foreign-library-directories*
      '("c:/Users/xxxx/quicklisp/local-projects/colitrsynth/dll/"
        "c:/msys64/mingw64/bin/"))
```

### SDL2_ttf.dll

MSYS2 の SDL2_ttf.dll はロードできなかったので
https://github.com/libsdl-org/SDL_ttf/releases
からダウンロードし dll ディレクトリに置いた。


### PortAudio

mingw-w64-x86_64-portaudio は ASIO サポートが有効になっていないみたいなので
PortAudio を自分でコンパイル

portaudio\src\hostapi\asio に ASIO のコードをコピー。
portaudio\build\msvc\portaudio.sln を開く。
プリプロセッサで `PA_USE_ASIO=1` などを設定。
Release ビルド。
portaudio\build\msvc\x64\Release\portaudio_x64.dll を
local-projects/colitrsynth/dll/ にコピー。

# TODO

- 名前を付けて保存
- 同じモジュール同士を2本以上のケーブルでつないでいるとケーブルが重なってしまうので、ケーブルをずらして描画したい
- LFO の周波数を変えたとき値がなめらかに変化するように
- ケーブル揺らすとか音の流れを視覚化したい
- ビルトインモジュールのモジュレーション
- パターンエディタ下にスクロールしているときクリックしてもフォーカスしない
- ケーブルを横切るようにドラッグで外したい
- 再生に合わせてシーケンサーの横スクロール
- オーディオデバイスの選択
- タイムラインは縦スクロール固定にしたい
- ミクさんに歌ってもらうにはレイテンシー対応したいとだめかも
- bypass
- ループ範囲をプラグインに渡す
- OFF は pattern をまたがって処理する必要があるのでは
- 先頭の音が出ない場合がある
- シーケンサーのズーム
- パターン編集でのコピペ
- レイアウトは resized で実装したい
- コード FX https://youtu.be/o4Mtsh3GWiU
- WAV の書き出し
- プラグインのレイテンシー
- セーブの必要があるか表示。「*」つける？
- 閉じるとき、別のファイル開くとき、セーブするか尋ねる。
- Undo & Redo

# DONE

- ループしたとき手抜きで 0 フレームから再生にしているので MDrummer とかずれちゃう
- テンポ上げると MIDI イベント欠落する
    - floor は最後に
- シーケンサーのスクロール
- lepis は使わない方がいいかな
    - view と model をサイド統合
- モジュールのコピペ
- スクロールしたときループ範囲してのドラッグ位置がずれる
- サンプルレートのハードコードをやめる
- サイドチェイン
- プラグインのモジュレーション
- トラックからパッチするときパターン貼り付けちゃうので、はやくなんとかして
- プラグインからのパラアウト
- モジュールを背面に送る
- pattern editor 上の方に配置するとゆがむ
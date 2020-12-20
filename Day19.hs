{-# LANGUAGE QuasiQuotes #-}

module Day19 (day19) where

import Data.String.Interpolate ( i )

mainRule :: String
mainRule =
  let
    rule19 = [i|(?:#{rule33}#{rule53}|#{rule123}#{rule7})|] :: String
    rule3 = [i|(?:#{rule33}#{rule82}|#{rule123}#{rule45})|] :: String
    rule92 = [i|(?:#{rule47}#{rule123}|#{rule91}#{rule33})|] :: String
    rule34 = [i|(?:#{rule123}#{rule60}|#{rule33}#{rule63})|] :: String
    rule91 = [i|(?:#{rule123}#{rule9}|#{rule33}#{rule7})|] :: String
    rule20 = [i|(?:#{rule33}#{rule46}|#{rule123}#{rule79})|] :: String
    rule101 = [i|(?:#{rule33}#{rule27}|#{rule123}#{rule56})|] :: String
    rule47 = [i|(?:#{rule52}#{rule33}|#{rule84}#{rule123})|] :: String
    rule115 = [i|(?:#{rule116}#{rule33}|#{rule7}#{rule123})|] :: String
    rule41 = [i|(?:#{rule57}#{rule33}|#{rule127}#{rule123})|] :: String
    rule33 = [i|(?:a)|] :: String
    rule109 = [i|(?:#{rule123}#{rule89}|#{rule33}#{rule33})|] :: String
    rule106 = [i|(?:#{rule33}#{rule72}|#{rule123}#{rule6})|] :: String
    rule8 = [i|(?:#{rule42})+|] :: String
    rule104 = [i|(?:#{rule92}#{rule123}|#{rule41}#{rule33})|] :: String
    rule75 = [i|(?:#{rule123}#{rule22}|#{rule33}#{rule100})|] :: String
    rule21 = [i|(?:#{rule76}#{rule123}|#{rule28}#{rule33})|] :: String
    rule102 = [i|(?:#{rule95}#{rule33}|#{rule81}#{rule123})|] :: String
    rule54 = [i|(?:#{rule33}#{rule44}|#{rule123}#{rule2})|] :: String
    rule77 = [i|(?:#{rule123}#{rule13}|#{rule33}#{rule116})|] :: String
    rule1 = [i|(?:#{rule33}#{rule26}|#{rule123}#{rule34})|] :: String
    rule83 = [i|(?:#{rule123}#{rule90}|#{rule33}#{rule66})|] :: String
    rule11 = [i|(#{rule42}#{rule31}|#{rule42}(?-1)#{rule31})|] :: String
    rule58 = [i|(?:#{rule9}#{rule33}|#{rule63}#{rule123})|] :: String
    rule136 = [i|(?:#{rule33}#{rule74}|#{rule123}#{rule109})|] :: String
    rule25 = [i|(?:#{rule59}#{rule123}|#{rule52}#{rule33})|] :: String
    rule17 = [i|(?:#{rule123}#{rule91}|#{rule33}#{rule136})|] :: String
    rule116 = [i|(?:#{rule89}#{rule89})|] :: String
    rule32 = [i|(?:#{rule116}#{rule33}|#{rule53}#{rule123})|] :: String
    rule72 = [i|(?:#{rule9}#{rule123}|#{rule111}#{rule33})|] :: String
    rule70 = [i|(?:#{rule33}#{rule9})|] :: String
    rule80 = [i|(?:#{rule33}#{rule20}|#{rule123}#{rule1})|] :: String
    rule18 = [i|(?:#{rule111}#{rule33}|#{rule74}#{rule123})|] :: String
    rule86 = [i|(?:#{rule33}#{rule122}|#{rule123}#{rule55})|] :: String
    rule14 = [i|(?:#{rule33}#{rule108}|#{rule123}#{rule37})|] :: String
    rule84 = [i|(?:#{rule33}#{rule33}|#{rule123}#{rule123})|] :: String
    rule67 = [i|(?:#{rule6}#{rule123}|#{rule18}#{rule33})|] :: String
    rule78 = [i|(?:#{rule123}#{rule58}|#{rule33}#{rule96})|] :: String
    rule38 = [i|(?:#{rule111}#{rule123}|#{rule52}#{rule33})|] :: String
    rule119 = [i|(?:#{rule125}#{rule33}|#{rule77}#{rule123})|] :: String
    rule69 = [i|(?:#{rule33}#{rule32}|#{rule123}#{rule64})|] :: String
    rule10 = [i|(?:#{rule110}#{rule123}|#{rule3}#{rule33})|] :: String
    rule118 = [i|(?:#{rule69}#{rule33}|#{rule107}#{rule123})|] :: String
    rule85 = [i|(?:#{rule123}#{rule62}|#{rule33}#{rule17})|] :: String
    rule61 = [i|(?:#{rule33}#{rule117}|#{rule123}#{rule93})|] :: String
    rule62 = [i|(?:#{rule70}#{rule123}|#{rule117}#{rule33})|] :: String
    rule4 = [i|(?:#{rule123}#{rule52}|#{rule33}#{rule7})|] :: String
    rule52 = [i|(?:#{rule123}#{rule33})|] :: String
    rule40 = [i|(?:#{rule84}#{rule123}|#{rule60}#{rule33})|] :: String
    rule49 = [i|(?:#{rule123}#{rule47}|#{rule33}#{rule65})|] :: String
    rule79 = [i|(?:#{rule13}#{rule33}|#{rule84}#{rule123})|] :: String
    rule6 = [i|(?:#{rule7}#{rule33}|#{rule116}#{rule123})|] :: String
    rule113 = [i|(?:#{rule33}#{rule133}|#{rule123}#{rule115})|] :: String
    rule98 = [i|(?:#{rule116}#{rule89})|] :: String
    rule53 = [i|(?:#{rule123}#{rule123})|] :: String
    rule42 = [i|(?:#{rule33}#{rule35}|#{rule123}#{rule21})|] :: String
    rule44 = [i|(?:#{rule33}#{rule15}|#{rule123}#{rule55})|] :: String
    rule100 = [i|(?:#{rule123}#{rule67}|#{rule33}#{rule121})|] :: String
    rule27 = [i|(?:#{rule88}#{rule33}|#{rule9}#{rule123})|] :: String
    rule76 = [i|(?:#{rule33}#{rule23}|#{rule123}#{rule85})|] :: String
    rule16 = [i|(?:#{rule33}#{rule24}|#{rule123}#{rule73})|] :: String
    rule9 = [i|(?:#{rule123}#{rule33}|#{rule33}#{rule123})|] :: String
    rule26 = [i|(?:#{rule109}#{rule123}|#{rule59}#{rule33})|] :: String
    rule36 = [i|(?:#{rule7}#{rule33}|#{rule52}#{rule123})|] :: String
    rule50 = [i|(?:#{rule123}#{rule88}|#{rule33}#{rule63})|] :: String
    rule65 = [i|(?:#{rule59}#{rule33}|#{rule84}#{rule123})|] :: String
    rule105 = [i|(?:#{rule9}#{rule123}|#{rule7}#{rule33})|] :: String
    rule112 = [i|(?:#{rule74}#{rule33}|#{rule60}#{rule123})|] :: String
    rule120 = [i|(?:#{rule111}#{rule123}|#{rule84}#{rule33})|] :: String
    rule23 = [i|(?:#{rule106}#{rule33}|#{rule126}#{rule123})|] :: String
    rule110 = [i|(?:#{rule104}#{rule123}|#{rule102}#{rule33})|] :: String
    rule128 = [i|(?:#{rule33}#{rule12}|#{rule123}#{rule5})|] :: String
    rule31 = [i|(?:#{rule123}#{rule114}|#{rule33}#{rule10})|] :: String
    rule30 = [i|(?:#{rule57}#{rule123}|#{rule98}#{rule33})|] :: String
    rule82 = [i|(?:#{rule33}#{rule51}|#{rule123}#{rule128})|] :: String
    rule125 = [i|(?:#{rule7}#{rule123}|#{rule9}#{rule33})|] :: String
    rule99 = [i|(?:#{rule123}#{rule120}|#{rule33}#{rule93})|] :: String
    rule87 = [i|(?:#{rule52}#{rule123}|#{rule59}#{rule33})|] :: String
    rule37 = [i|(?:#{rule33}#{rule101}|#{rule123}#{rule16})|] :: String
    rule43 = [i|(?:#{rule123}#{rule7}|#{rule33}#{rule88})|] :: String
    rule117 = [i|(?:#{rule123}#{rule63}|#{rule33}#{rule7})|] :: String
    rule12 = [i|(?:#{rule33}#{rule88}|#{rule123}#{rule53})|] :: String
    rule55 = [i|(?:#{rule33}#{rule111})|] :: String
    rule2 = [i|(?:#{rule123}#{rule132}|#{rule33}#{rule38})|] :: String
    rule96 = [i|(?:#{rule33}#{rule116}|#{rule123}#{rule7})|] :: String
    rule135 = [i|(?:#{rule33}#{rule109}|#{rule123}#{rule111})|] :: String
    rule126 = [i|(?:#{rule112}#{rule123}|#{rule36}#{rule33})|] :: String
    rule97 = [i|(?:#{rule111}#{rule33}|#{rule60}#{rule123})|] :: String
    rule22 = [i|(?:#{rule33}#{rule113}|#{rule123}#{rule48})|] :: String
    rule24 = [i|(?:#{rule74}#{rule33}|#{rule84}#{rule123})|] :: String
    rule130 = [i|(?:#{rule91}#{rule33}|#{rule103}#{rule123})|] :: String
    rule132 = [i|(?:#{rule33}#{rule109}|#{rule123}#{rule53})|] :: String
    rule59 = [i|(?:#{rule33}#{rule89}|#{rule123}#{rule123})|] :: String
    rule51 = [i|(?:#{rule123}#{rule40}|#{rule33}#{rule50})|] :: String
    rule28 = [i|(?:#{rule54}#{rule123}|#{rule68}#{rule33})|] :: String
    rule66 = [i|(?:#{rule33}#{rule111}|#{rule123}#{rule52})|] :: String
    rule64 = [i|(?:#{rule13}#{rule33}|#{rule9}#{rule123})|] :: String
    rule15 = [i|(?:#{rule33}#{rule13}|#{rule123}#{rule111})|] :: String
    rule114 = [i|(?:#{rule14}#{rule123}|#{rule75}#{rule33})|] :: String
    rule35 = [i|(?:#{rule123}#{rule131}|#{rule33}#{rule39})|] :: String
    rule90 = [i|(?:#{rule60}#{rule123}|#{rule52}#{rule33})|] :: String
    rule124 = [i|(?:#{rule33}#{rule32}|#{rule123}#{rule71})|] :: String
    rule63 = [i|(?:#{rule123}#{rule123}|#{rule33}#{rule123})|] :: String
    rule121 = [i|(?:#{rule33}#{rule97}|#{rule123}#{rule19})|] :: String
    rule71 = [i|(?:#{rule116}#{rule123}|#{rule53}#{rule33})|] :: String
    rule5 = [i|(?:#{rule33}#{rule53}|#{rule123}#{rule63})|] :: String
    rule111 = [i|(?:#{rule33}#{rule33}|#{rule123}#{rule33})|] :: String
    rule74 = [i|(?:#{rule33}#{rule123}|#{rule33}#{rule33})|] :: String
    rule48 = [i|(?:#{rule94}#{rule123}|#{rule105}#{rule33})|] :: String
    rule56 = [i|(?:#{rule111}#{rule33}|#{rule84}#{rule123})|] :: String
    rule29 = [i|(?:#{rule30}#{rule33}|#{rule130}#{rule123})|] :: String
    rule45 = [i|(?:#{rule99}#{rule123}|#{rule119}#{rule33})|] :: String
    rule93 = [i|(?:#{rule52}#{rule33}|#{rule52}#{rule123})|] :: String
    rule122 = [i|(?:#{rule33}#{rule9}|#{rule123}#{rule7})|] :: String
    rule103 = [i|(?:#{rule13}#{rule33}|#{rule63}#{rule123})|] :: String
    rule68 = [i|(?:#{rule61}#{rule33}|#{rule49}#{rule123})|] :: String
    rule94 = [i|(?:#{rule33}#{rule88}|#{rule123}#{rule129})|] :: String
    rule129 = [i|(?:#{rule89}#{rule33}|#{rule33}#{rule123})|] :: String
    rule133 = [i|(?:#{rule123}#{rule59}|#{rule33}#{rule129})|] :: String
    rule127 = [i|(?:#{rule33}#{rule74}|#{rule123}#{rule59})|] :: String
    rule0 = [i|(?:#{rule8}#{rule11})|] :: String
    rule134 = [i|(?:#{rule78}#{rule123}|#{rule83}#{rule33})|] :: String
    rule73 = [i|(?:#{rule111}#{rule33}|#{rule116}#{rule123})|] :: String
    rule89 = [i|(?:#{rule33}|#{rule123})|] :: String
    rule123 = [i|(?:b)|] :: String
    rule131 = [i|(?:#{rule80}#{rule123}|#{rule134}#{rule33})|] :: String
    rule81 = [i|(?:#{rule123}#{rule43}|#{rule33}#{rule56})|] :: String
    rule7 = [i|(?:#{rule123}#{rule33}|#{rule123}#{rule123})|] :: String
    rule13 = [i|(?:#{rule123}#{rule33}|#{rule89}#{rule123})|] :: String
    rule60 = [i|(?:#{rule33}#{rule33})|] :: String
    rule108 = [i|(?:#{rule124}#{rule123}|#{rule86}#{rule33})|] :: String
    rule39 = [i|(?:#{rule123}#{rule29}|#{rule33}#{rule118})|] :: String
    rule95 = [i|(?:#{rule123}#{rule87}|#{rule33}#{rule135})|] :: String
    rule88 = [i|(?:#{rule33}#{rule123})|] :: String
    rule57 = [i|(?:#{rule123}#{rule60}|#{rule33}#{rule9})|] :: String
    rule107 = [i|(?:#{rule123}#{rule4}|#{rule33}#{rule25})|] :: String
    rule46 = [i|(?:#{rule123}#{rule60}|#{rule33}#{rule7})|] :: String
  in
    rule0

day19 :: IO ()
day19 = do
  putStrLn mainRule
  pure ()

# --- 1. Data Loading and Preparation ---

# Weapon Categories - Defined upfront for clarity and reuse
semiAuto <- c("M1 CARBINE","GEWEHR 43","M1 GARAND","M97 TRENCH GUN","STG44","LUGER P08","COLT M1911","WALTHER P38")
MG <- c("MG42","MG34","BROWNING M1919")
Automatic <- c("MP40","M1A1 THOMPSON","M3 GREASE GUN","M1918A2 BAR","FG42")
Sniper <- c("M1903 SPRINGFIELD","KARABINER 98K x8","FG42 x4")
Arty <- c("150MM HOWITZER [sFH 18]","155MM HOWITZER [M114]")
BoltAction <- c("KARABINER 98K")
Grenade <- c("MK2 GRENADE","M43 STIELHANDGRANATE","M24 STIELHANDGRANATE")
AT <- c("BAZOOKA","PANZERSCHRECK")
Satchel <- c("SATCHEL")
AT_Gun<- c("75MM CANNON [PAK 40]")
Mine <- c("M2 AP MINE","TELLERMINE 43","S-MINE","M1A1 AT MINE")
Melee <- c("M3 KNIFE","FELDSPATEN")
Armor <- c("M6 37mm [M8 Greyhound]", "COAXIAL M1919 [M8 Greyhound]", "M8 Greyhound",
           "37MM CANNON [Stuart M5A1]", "COAXIAL M1919 [Stuart M5A1]", "HULL M1919 [Stuart M5A1]", "Stuart M5A1",
           "75MM CANNON [Sherman M4A3(75)W]", "COAXIAL M1919 [Sherman M4A3(75)W]", "HULL M1919 [Sherman M4A3(75)W]", "Sherman M4A3(75)W",
           "75MM M3 GUN [Sherman M4A3E2]", "COAXIAL M1919 [Sherman M4A3E2]", "HULL M1919 [Sherman M4A3E2]", "Sherman M4A3E2",
           "76MM M1 GUN [Sherman M4A3E2(76)]", "COAXIAL M1919 [Sherman M4A3E2(76)]", "HULL M1919 [Sherman M4A3E2(76)]", "Sherman M4A3E2(76]",
           "50mm KwK 39/1 [Sd.Kfz.234 Puma]", "COAXIAL MG34 [Sd.Kfz.234 Puma]", "Sd.Kfz.234 Puma",
           "20MM KWK 30 [Sd.Kfz.121 Luchs]", "COAXIAL MG34 [Sd.Kfz.121 Luchs]", "Sd.Kfz.121 Luchs",
           "75MM CANNON [Sd.Kfz.161 Panzer IV]", "COAXIAL MG34 [Sd.Kfz.161 Panzer IV]", "HULL MG34 [Sd.Kfz.161 Panzer IV]", "Sd.Kfz.161 Panzer IV",
           "88 KWK 36 L/56 [Sd.Kfz.181 Tiger 1]", "COAXIAL MG34 [Sd.Kfz.181 Tiger 1]", "HULL MG34 [Sd.Kfz.181 Tiger 1]", "Sd.Kfz.181 Tiger 1",
           "75MM CANNON [Sd.Kfz.171 Panther]", "COAXIAL MG34 [Sd.Kfz.171 Panther]", "HULL MG34 [Sd.Kfz.171 Panther]", "Sd.Kfz.171 Panther",
           "COAXIAL MG34"
)
Command <- c("BOMBING RUN","STRAFING RUN","PRECISION STRIKE")
Other <- c("UNKNOWN","M2 FLAMETHROWER", "GMC CCKW 353 (Transport)","Opel Blitz (Transport)", "M1919 SPRINGFIELD", "FLARE GUN", "M3 Half-track", "GMC CCKW 363 (Supply)", "GMC CCKW 353 (Supply)", "GMC CCKW 363 (Transport)", "Jeep Willys", "FLAMMENWERFER 41", "Kubelwagen", "MG 42 [Sd.Kfz 251 Half-track]")

AXIS_WEAPONS = c('MP40', 'GEWEHR 43', 'KARABINER 98K', 'STG44', 'FG42', 'MG34', 'MG42', 'KARABINER 98K x8', 'FG42 x4', 'WALTHER P38', 'LUGER P08', 'FLAMMENWERFER 41', 'FELDSPATEN', 'M24 STIELHANDGRANATE', 'M43 STIELHANDGRANATE', 'S-MINE', 'TELLERMINE 43', 'PANZERSCHRECK', 'FLARE GUN', '150MM HOWITZER [sFH 18]','Sd.Kfz.234 Puma', 'Sd.Kfz.121 Luchs', 'Sd.Kfz.161 Panzer IV', 'Sd.Kfz.181 Tiger 1', 'Sd.Kfz.171 Panther', 'Sd.Kfz 251 Half-track', 'Opel Blitz (Transport)', 'Opel Blitz (Supply)', 'Kubelwagen', '50mm KwK 39/1 [Sd.Kfz.234 Puma]', 'COAXIAL MG34', 'COAXIAL MG34 [Sd.Kfz.234 Puma]', '20MM KWK 30 [Sd.Kfz.121 Luchs]', 'COAXIAL MG34 [Sd.Kfz.121 Luchs]', '75MM CANNON [Sd.Kfz.161 Panzer IV]', 'COAXIAL MG34 [Sd.Kfz.161 Panzer IV]', 'HULL MG34 [Sd.Kfz.161 Panzer IV]', '75MM CANNON [Sd.Kfz.171 Panther]', 'COAXIAL MG34 [Sd.Kfz.171 Panther]', 'HULL MG34 [Sd.Kfz.171 Panther]', '88 KWK 36 L/56 [Sd.Kfz.181 Tiger 1]', 'COAXIAL MG34 [Sd.Kfz.181 Tiger 1]', 'HULL MG34 [Sd.Kfz.181 Tiger 1]', 'MG 42 [Sd.Kfz 251 Half-track]')
US_WEAPONS = c('M1A1 THOMPSON', 'M3 GREASE GUN', 'M1 GARAND', 'M1 CARBINE', 'M1918A2 BAR', 'M97 TRENCH GUN', 'BROWNING M1919', 'M1919 SPRINGFIELD', 'M1903 SPRINGFIELD', 'COLT M1911', 'M2 FLAMETHROWER', 'M3 KNIFE', 'MK2 GRENADE', 'M2 AP MINE', 'M1A1 AT MINE', 'BAZOOKA', 'FLARE GUN', '155MM HOWITZER [M114]', '57MM CANNON [M1 57mm]', 'M8 Greyhound', 'Stuart M5A1', 'Sherman M4A3(75)W', 'Sherman M4A3E2', 'Sherman M4A3E2(76)', 'M3 Half-track', 'GMC CCKW 363 (Transport)', 'GMC CCKW 363 (Supply)', 'GMC CCKW 353 (Supply)', 'Jeep Willys', 'M6 37mm [M8 Greyhound]', 'COAXIAL M1919 [M8 Greyhound]', '37MM CANNON [Stuart M5A1]', 'COAXIAL M1919 [Stuart M5A1]', 'HULL M1919 [Stuart M5A1]', '75MM CANNON [Sherman M4A3(75)W]', 'COAXIAL M1919 [Sherman M4A3(75)W]', 'HULL M1919 [Sherman M4A3(75)W]', '75MM M3 GUN [Sherman M4A3E2]', 'COAXIAL M1919 [Sherman M4A3E2]', 'HULL M1919 [Sherman M4A3E2]', '76MM M1 GUN [Sherman M4A3E2(76)]', 'COAXIAL M1919 [Sherman M4A3E2(76)]', 'HULL M1919 [Sherman M4A3E2(76)]', 'M2 Browning [M3 Half-track]')

weapon_types_regular <- c("SemiAuto", "Automatic", "MG", "Bolt-Action")
weapon_types_util <- c("Satchel", "AT", "Mine", "Grenade", "AT-Gun", "Melee", "Command")

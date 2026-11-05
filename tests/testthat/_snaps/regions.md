# printout is correct

    Code
      states()
    Output
      States
      ------ 
      * Abia
      * Adamawa
      * Akwa Ibom
      * Anambra
      * Bauchi
      * Bayelsa
      * Benue
      * Borno
      * Cross River
      * Delta
      * Ebonyi
      * Edo
      * Ekiti
      * Enugu
      * Federal Capital Territory
      * Gombe
      * Imo
      * Jigawa
      * Kaduna
      * Kano
      * Katsina
      * Kebbi
      * Kogi
      * Kwara
      * Lagos
      * Nasarawa
      * Niger
      * Ogun
      * Ondo
      * Osun
      * Oyo
      * Plateau
      * Rivers
      * Sokoto
      * Taraba
      * Yobe
      * Zamfara 

---

    Code
      states("Akwa Ibom")
    Output
      States
      ------ 
      * Akwa Ibom 

---

    Code
      states(c("Abia", "Kaduna", "Bauchi"))
    Output
      States
      ------ 
      * Abia
      * Bauchi
      * Kaduna 

---

    Code
      states(c("Avia", "Dalta", "Keduna", "Dulta"))
    Condition
      Warning:
      One or more items is not a State. Spelling error?
    Output
      States
      ------ 
      * Avia
      * Dalta
      * Dulta
      * Keduna 

# printout for LGAs is correct

    Code
      lgas()
    Output
      Local Government Areas
      ---------------------- 
      * Aba North
      * Aba South
      * Abadam
      * Abaji
      * Abak
      * Abakaliki
      * Abeokuta North
      * Abeokuta South
      * Abi
      * Aboh Mbaise
      * Abua/Odual
      * Abuja Municipal
      * Adavi
      * Ado
      * Ado-Ekiti
      * Ado-Odo/Ota
      * Afijio
      * Afikpo North
      * Afikpo South
      * Agaie
      * Agatu
      * Agege
      * Aguata
      * Agwara
      * Ahiazu Mbaise
      * Ahoada East
      * Ahoada West
      * Aiyedaade
      * Aiyedire
      * Ajaokuta
      * Ajeromi-Ifelodun
      * Ajingi
      * Akamkpa
      * Akinyele
      * Akko
      * Akoko Edo
      * Akoko North-East
      * Akoko North-West
      * Akoko South-East
      * Akoko South-West
      * Akpabuyo
      * Akuku-Toru
      * Akure North
      * Akure South
      * Akwanga
      * Albasu
      * Aleiro
      * Alimosho
      * Alkaleri
      * Amuwo-Odofin
      * Anambra East
      * Anambra West
      * Anaocha
      * Andoni
      * Aninri
      * Aniocha North
      * Aniocha South
      * Anka
      * Ankpa
      * Apa
      * Apapa
      * Ardo Kola
      * Arewa Dandi
      * Argungu
      * Arochukwu
      * Asa
      * Asari-Toru
      * Askira/Uba
      * Atakunmosa East
      * Atakunmosa West
      * Atiba
      * Atisbo
      * Augie
      * Auyo
      * Awe
      * Awgu
      * Awka North
      * Awka South
      * Ayamelum
      * Babura
      * Badagry
      * Bade
      * Bagudo
      * Bagwai
      * Bakassi
      * Bakori
      * Bakura
      * Balanga
      * Bali
      * Bama
      * Barkin Ladi
      * Baruten
      * Bassa
      * Batagarawa
      * Batsari
      * Bauchi
      * Baure
      * Bayo
      * Bebeji
      * Bekwarra
      * Bende
      * Biase
      * Bichi
      * Bida
      * Billiri
      * Bindawa
      * Binji
      * Biriniwa
      * Birnin Gwari
      * Birnin Kebbi
      * Birnin Kudu
      * Birnin Magaji/Kiyaw
      * Biu
      * Bodinga
      * Bogoro
      * Boki
      * Bokkos
      * Boluwaduro
      * Bomadi
      * Bonny
      * Borgu
      * Boripe
      * Bosso
      * Brass
      * Buji
      * Bukkuyum
      * Bungudu
      * Bunkure
      * Bunza
      * Bursari
      * Buruku
      * Burutu
      * Bwari
      * Calabar Municipal
      * Calabar South
      * Chanchaga
      * Charanchi
      * Chibok
      * Chikun
      * Dala
      * Damaturu
      * Damban
      * Dambatta
      * Damboa
      * Dan Musa
      * Dandi
      * Dandume
      * Dange Shuni
      * Danja
      * Darazo
      * Dass
      * Daura
      * Dawakin Kudu
      * Dawakin Tofa
      * Degema
      * Dekina
      * Demsa
      * Dikwa
      * Doguwa
      * Doma
      * Donga
      * Dukku
      * Dunukofia
      * Dutse
      * Dutsi
      * Dutsin-Ma
      * Eastern Obolo
      * Ebonyi
      * Edati
      * Ede North
      * Ede South
      * Edu
      * Efon
      * Egbeda
      * Egbedore
      * Egor
      * Ehime Mbano
      * Ejigbo
      * Ekeremor
      * Eket
      * Ekiti
      * Ekiti East
      * Ekiti South-West
      * Ekiti West
      * Ekwusigo
      * Eleme
      * Emohua
      * Emure
      * Enugu East
      * Enugu North
      * Enugu South
      * Epe
      * Esan Central
      * Esan North-East
      * Esan South-East
      * Esan West
      * Ese Odo
      * Esit Eket
      * Essien Udim
      * Etche
      * Ethiope East
      * Ethiope West
      * Eti-Osa
      * Etim Ekpo
      * Etinan
      * Etsako Central
      * Etsako East
      * Etsako West
      * Etung
      * Ewekoro
      * Ezeagu
      * Ezinihitte-Mbaise
      * Ezza North
      * Ezza South
      * Fagge
      * Fakai
      * Faskari
      * Fika
      * Fufore
      * Funakaye
      * Fune
      * Funtua
      * Gabasawa
      * Gada
      * Gagarawa
      * Gamawa
      * Ganjuwa
      * Ganye
      * Garki
      * Garko
      * Garun Mallam
      * Gashaka
      * Gassol
      * Gaya
      * Gbako
      * Gboko
      * Gbonyin
      * Geidam
      * Gezawa
      * Giade
      * Girei
      * Giwa
      * Gokana
      * Gombe
      * Gombi
      * Goronyo
      * Gubio
      * Gudu
      * Gujba
      * Gulani
      * Guma
      * Gumel
      * Gummi
      * Gurara
      * Guri
      * Gusau
      * Guyuk
      * Guzamala
      * Gwadabawa
      * Gwagwalada
      * Gwale
      * Gwandu
      * Gwaram
      * Gwarzo
      * Gwer East
      * Gwer West
      * Gwiwa
      * Gwoza
      * Hadejia
      * Hawul
      * Hong
      * Ibadan North
      * Ibadan North-East
      * Ibadan North-West
      * Ibadan South-East
      * Ibadan South-West
      * Ibaji
      * Ibarapa Central
      * Ibarapa East
      * Ibarapa North
      * Ibeju-Lekki
      * Ibeno
      * Ibesikpo Asutan
      * Ibi
      * Ibiono-Ibom
      * Idah
      * Idanre
      * Ideato North
      * Ideato South
      * Idemili North
      * Idemili South
      * Ido
      * Ido-Osi
      * Ifako-Ijaiye
      * Ife Central
      * Ife East
      * Ife North
      * Ife South
      * Ifedayo
      * Ifedore
      * Ifelodun
      * Ifo
      * Igabi
      * Igalamela-Odolu
      * Igbo Etiti
      * Igbo Eze North
      * Igbo Eze South
      * Igueben
      * Ihiala
      * Ihitte/Uboma
      * Ijebu East
      * Ijebu North
      * Ijebu North East
      * Ijebu Ode
      * Ijero
      * Ijumu
      * Ika
      * Ika North East
      * Ika South
      * Ikara
      * Ikeduru
      * Ikeja
      * Ikenne
      * Ikere
      * Ikole
      * Ikom
      * Ikono
      * Ikorodu
      * Ikot Ekpene
      * Ikot-Abasi
      * Ikpoba Okha
      * Ikwerre
      * Ikwo
      * Ikwuano
      * Ila
      * Ilaje
      * Ile Oluji/Okeigbo
      * Ilejemeje
      * Ilesa East
      * Ilesa West
      * Illela
      * Ilorin-East
      * Ilorin-South
      * Ilorin-West
      * Imeko Afon
      * Ingawa
      * Ini
      * Ipokia
      * Irele
      * Irepo
      * Irepodun
      * Irepodun/Ifelodun
      * Irewole
      * Isa
      * Ise/Orun
      * Iseyin
      * Ishielu
      * Isi Uzo
      * Isiala Mbano
      * Isiala-Ngwa North
      * Isiala-Ngwa South
      * Isin
      * Isokan
      * Isoko North
      * Isoko South
      * Isu
      * Isuikwuato
      * Itas/Gadau
      * Itesiwaju
      * Itu
      * Ivo
      * Iwajowa
      * Iwo
      * Izzi
      * Jaba
      * Jada
      * Jahun
      * Jakusko
      * Jalingo
      * Jama'are
      * Jega
      * Jema'a
      * Jere
      * Jibia
      * Jos East
      * Jos North
      * Jos South
      * Kabba/Bunu
      * Kabo
      * Kachia
      * Kaduna North
      * Kaduna South
      * Kafin Hausa
      * Kafur
      * Kaga
      * Kagarko
      * Kaiama
      * Kaita
      * Kajola
      * Kajuru
      * Kala/Balge
      * Kalgo
      * Kaltungo
      * Kanam
      * Kankara
      * Kanke
      * Kankia
      * Kano Municipal
      * Karasuwa
      * Karaye
      * Karim Lamido
      * Karu
      * Katagum
      * Katcha
      * Katsina
      * Katsina-Ala
      * Kaugama
      * Kaura
      * Kaura Namoda
      * Kauru
      * Kazaure
      * Keana
      * Kebbe
      * Keffi
      * Khana
      * Kibiya
      * Kirfi
      * Kiri Kasama
      * Kiru
      * Kiyawa
      * Kogi
      * Koko/Besse
      * Kokona
      * Kolokuma/Opokuma
      * Konduga
      * Konshisha
      * Kontagora
      * Kosofe
      * Kubau
      * Kudan
      * Kuje
      * Kukawa
      * Kumbotso
      * Kunchi
      * Kura
      * Kurfi
      * Kurmi
      * Kusada
      * Kwali
      * Kwami
      * Kwande
      * Kware
      * Kwaya Kusar
      * Lafia
      * Lagelu
      * Lagos Island
      * Lagos Mainland
      * Lamurde
      * Langtang North
      * Langtang South
      * Lapai
      * Lau
      * Lavun
      * Lere
      * Logo
      * Lokoja
      * Machina
      * Madagali
      * Madobi
      * Mafa
      * Magama
      * Magumeri
      * Mai'Adua
      * Maiduguri
      * Maigatari
      * Maiha
      * Maiyama
      * Makarfi
      * Makoda
      * Makurdi
      * Malam Madori
      * Malumfashi
      * Mangu
      * Mani
      * Maradun
      * Mariga
      * Marte
      * Maru
      * Mashegu
      * Mashi
      * Matazu
      * Mayo Belwa
      * Mbaitoli
      * Mbo
      * Michika
      * Miga
      * Mikang
      * Minjibir
      * Misau
      * Mkpat-Enin
      * Moba
      * Mobbar
      * Mokwa
      * Monguno
      * Mopa-Muro
      * Moro
      * Mubi North
      * Mubi South
      * Munya
      * Musawa
      * Mushin
      * Nafada
      * Nangere
      * Nasarawa
      * Nasarawa Egon
      * Ndokwa East
      * Ndokwa West
      * Nembe
      * Ngala
      * Nganzai
      * Ngaski
      * Ngor Okpala
      * Nguru
      * Ningi
      * Njaba
      * Njikoka
      * Nkanu East
      * Nkanu West
      * Nkwerre
      * Nnewi North
      * Nnewi South
      * Nsit Ubium
      * Nsit-Atai
      * Nsit-Ibom
      * Nsukka
      * Numan
      * Nwangele
      * Obafemi Owode
      * Obanliku
      * Obi
      * Obi Ngwa
      * Obia/Akpor
      * Obokun
      * Obot-Akara
      * Obowo
      * Obubra
      * Obudu
      * Odeda
      * Odigbo
      * Odo Otin
      * Odogbolu
      * Odukpani
      * Offa
      * Ofu
      * Ogba/Egbema/Ndoni
      * Ogbadibo
      * Ogbaru
      * Ogbia
      * Ogbomosho North
      * Ogbomosho South
      * Ogo Oluwa
      * Ogoja
      * Ogori/Magongo
      * Ogu/Bolo
      * Ogun Waterside
      * Oguta
      * Ohafia
      * Ohaji/Egbema
      * Ohaozara
      * Ohaukwu
      * Ohimini
      * Oji River
      * Ojo
      * Oju
      * Oke Ero
      * Okehi
      * Okene
      * Okigwe
      * Okitipupa
      * Okobo
      * Okpe
      * Okpokwu
      * Okrika
      * Ola Oluwa
      * Olamaboro
      * Olorunda
      * Olorunsogo
      * Oluyole
      * Omala
      * Omuma
      * Ona Ara
      * Ondo East
      * Ondo West
      * Onicha
      * Onitsha North
      * Onitsha South
      * Onna
      * Onuimo
      * Opobo/Nkoro
      * Oredo
      * Orelope
      * Orhionmwon
      * Ori Ire
      * Oriade
      * Orlu
      * Orolu
      * Oron
      * Orsu
      * Oru East
      * Oru West
      * Oruk Anam
      * Orumba North
      * Orumba South
      * Ose
      * Oshimili North
      * Oshimili South
      * Oshodi-Isolo
      * Osisioma Ngwa
      * Osogbo
      * Otukpo
      * Ovia North-East
      * Ovia South-West
      * Owan East
      * Owan West
      * Owerri Municipal
      * Owerri North
      * Owerri West
      * Owo
      * Oye
      * Oyi
      * Oyigbo
      * Oyo East
      * Oyo West
      * Oyun
      * Paikoro
      * Pankshin
      * Patani
      * Patigi
      * Port Harcourt
      * Potiskum
      * Qua'an Pan
      * Rabah
      * Rafi
      * Rano
      * Remo North
      * Rijau
      * Rimi
      * Rimin Gado
      * Ringim
      * Riyom
      * Rogo
      * Roni
      * Sabon Birni
      * Sabon Gari
      * Sabuwa
      * Safana
      * Sagamu
      * Sagbama
      * Sakaba
      * Saki East
      * Saki West
      * Sandamu
      * Sanga
      * Sapele
      * Sardauna
      * Shagari
      * Shanga
      * Shani
      * Shanono
      * Shelleng
      * Shendam
      * Shinkafi
      * Shira
      * Shiroro
      * Shomolu
      * Shongom
      * Silame
      * Soba
      * Sokoto North
      * Sokoto South
      * Song
      * Southern Ijaw
      * Sule Tankarkar
      * Suleja
      * Sumaila
      * Suru
      * Surulere
      * Tafa
      * Tafawa Balewa
      * Tai
      * Takai
      * Takum
      * Talata Mafara
      * Tambuwal
      * Tangaza
      * Tarauni
      * Tarka
      * Tarmuwa
      * Taura
      * Tofa
      * Toro
      * Toto
      * Toungo
      * Tsafe
      * Tsanyawa
      * Tudun Wada
      * Tureta
      * Udenu
      * Udi
      * Udu
      * Udung-Uko
      * Ughelli North
      * Ughelli South
      * Ugwunagbo
      * Uhunmwonde
      * Ukanafun
      * Ukum
      * Ukwa East
      * Ukwa West
      * Ukwuani
      * Umu-Nneochi
      * Umuahia North
      * Umuahia South
      * Ungogo
      * Uruan
      * Urue-Offong/Oruko
      * Ushongo
      * Ussa
      * Uvwie
      * Uyo
      * Uzo Uwani
      * Vandeikya
      * Wamako
      * Wamba
      * Warawa
      * Warji
      * Warri North
      * Warri South
      * Warri South West
      * Wasagu/Danko
      * Wase
      * Wudil
      * Wukari
      * Wurno
      * Wushishi
      * Yabo
      * Yagba East
      * Yagba West
      * Yakurr
      * Yala
      * Yamaltu/Deba
      * Yankwashi
      * Yauri
      * Yenegoa
      * Yewa North
      * Yewa South
      * Yola North
      * Yola South
      * Yorro
      * Yunusari
      * Yusufari
      * Zaki
      * Zango
      * Zangon Kataf
      * Zaria
      * Zing
      * Zurmi
      * Zuru 

---

    Code
      lgas("Akwa Ibom")
    Output
      Local Government Areas
      ---------------------- 
      * Abak
      * Eastern Obolo
      * Eket
      * Esit Eket
      * Essien Udim
      * Etim Ekpo
      * Etinan
      * Ibeno
      * Ibesikpo Asutan
      * Ibiono-Ibom
      * Ika
      * Ikono
      * Ikot Ekpene
      * Ikot-Abasi
      * Ini
      * Itu
      * Mbo
      * Mkpat-Enin
      * Nsit Ubium
      * Nsit-Atai
      * Nsit-Ibom
      * Obot-Akara
      * Okobo
      * Onna
      * Oron
      * Oruk Anam
      * Udung-Uko
      * Ukanafun
      * Uruan
      * Urue-Offong/Oruko
      * Uyo 

---

    Code
      lgas(c("Fufore", "Demsa", "Fufore"))
    Output
      Local Government Areas
      ---------------------- 
      * Demsa
      * Fufore 

---

    Code
      lgas(c("Fafure", "Damsa", "Fufore", "Dumsa"))
    Condition
      Warning:
      One or more items is not an LGA. Spelling error?
    Output
      Local Government Areas
      ---------------------- 
      * Damsa
      * Dumsa
      * Fafure
      * Fufore 


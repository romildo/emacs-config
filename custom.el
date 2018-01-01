(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-font-list
   (quote
    ((1 "" "" "\\mathcal{" "}")
     (2 "\\textbf{" "}" "\\mathbf{" "}")
     (3 "\\textsc{" "}")
     (5 "\\emph{" "}")
     (6 "\\textsf{" "}" "\\mathsf{" "}")
     (9 "\\textit{" "}" "\\mathit{" "}")
     (11 "\\alert{" "}")
     (12 "\\clean{" "}")
     (13 "\\textmd{" "}")
     (14 "\\textnormal{" "}" "\\mathnormal{" "}")
     (16 "\\pyginline|" "|")
     (18 "\\textrm{" "}" "\\mathrm{" "}")
     (19 "\\textsl{" "}")
     (20 "\\texttt{" "}" "\\mathtt{" "}")
     (21 "\\textup{" "}")
     (22 "\\underline{" "}")
     (4 "" "" t))))
 '(LaTeX-item-indent 0)
 '(LaTeX-verbatim-environments
   (quote
    ("verbatim" "verbatim*" "minted" "pygmented" "Verbatim" "Verbatim*" "BVerbatim" "LVerbatim" "runexample")))
 '(LaTeX-verbatim-macros-with-delims (quote ("verb" "verb*" "pyginline")))
 '(Linum-format "%7i ")
 '(Man-notify-method (quote pushy))
 '(TeX-PDF-mode t)
 '(TeX-auto-default "/usr/lib/texmf/tex/auto/")
 '(TeX-auto-untabify nil)
 '(TeX-macro-default "/usr/share/texmf/tex/")
 '(TeX-macro-global
   (quote
    ("/usr/share/texmf/tex/" "/usr/local/share/texmf/tex/")))
 '(TeX-quote-after-quote t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-map (ansi-color-make-color-map) t)
 '(ansi-color-names-vector
   ["#f7f3e5" "#b34d6c" "#777e62" "#d7875f" "#4d6e66" "#685672" "#6b6254" "#4d5452"])
 '(ansi-term-color-vector
   [unspecified "#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#6c71c4" "#268bd2" "#eee8d5"] t)
 '(background-color "#202020")
 '(background-mode dark)
 '(beacon-color "#F8BBD0")
 '(c-basic-offset 3)
 '(c-offsets-alist (quote ((substatement-open . 0))))
 '(canlock-password "216ddf918333d5f28eb9b418bfdc99a14ead1dec")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   (quote
    ("8e2281b816edb0ec9ff44384662b68bfab8c5389c0258c447a3513fcb397a4f3" "d8004f25fd54dd59b4531dabc01ea52b53c06e2ed134d7f8ec141dfde97931bb" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "4ec95bc5c7a04d3f76ec70471e9a466290f0973a5b4abde298904d9cc320c35a" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "6c7db7fdf356cf6bde4236248b17b129624d397a8e662cf1264e41dab87a4a9a" "12b7ed9b0e990f6d41827c343467d2a6c464094cbcc6d0844df32837b50655f9" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "456ac8176c7c01680384356cbdc568a7683d1bada9c83ae3d7294809ddda4014" "1011be33e9843afd22d8d26b031fbbb59036b1ce537d0b250347c19e1bd959d0" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "810ab30a73c460f5c49ede85d1b9af3429ff2dff652534518fa1de7adc83d0f6" "f64c9f8b4241b680b186f4620afb9c82fa2a76cf4498a7431f90db59bb1892eb" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "3a53f98f4354d66ffaec1edce1bc54a3c622c8a73a583e90fde456b311b889f2" "341a1f149c8ab55893e5a065e96235e43ee9f82423f4c018bf31a430e1dc1b0f" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "d9b0d5c7077ddad798e9749d0629e86b925c2e146641b24130edd8f82cd0cf5d" "d01804789955754acb87fb5747a5f25dd405a2546d23d7eb92f26527f927e3ce" "e295b6ff676311b3434c9b9b7a1344ad6cd8f8b875685f758156239ec51d2b8d" "23562d67c3657a80dd1afc21e1e80652db0ff819e477649d23a38c1502d1245f" "b747fb36e99bc7f497248eafd6e32b45613ee086da74d1d92a8da59d37b9a829" "df3a4fbc0c536fbef14cc1ff22f88d0eddd259fbde470c0a4831c892860d4661" "b5106c46bc4e958eff4965a759a72bef9ba59d559f07754a966783b09e80e91b" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "9ab634dcc9131f79016c96c4955298409649f6538908c743a8a9d2c6bc8321ef" "5b29f90eb304b440c908de31caf7d730db451b5909e8a84a2e7cd8d60f6d5c1f" "d4e9f95acd51433b776f1127143bbc7d0f1d41112d547e5b7a9a506be369dc39" "ac194ceaf2a741572f99274e22c96c953f27de11ca4944c05f4e3252937c12a0" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "f81933744f47a010213537575f84085af3937b27748b4f5c9249c5e100856fc5" "613a7c50dbea57860eae686d580f83867582ffdadd63f0f3ebe6a85455ab7706" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "40c66989886b3f05b0c4f80952f128c6c4600f85b1f0996caa1fa1479e20c082" "715fdcd387af7e963abca6765bd7c2b37e76154e65401cd8d86104f22dd88404" "98a619757483dc6614c266107ab6b19d315f93267e535ec89b7af3d62fb83cad" "6a925fdf3a7bf2f3901d8fbc4ef64f9b4b4be2c6bed2b0d49d154db0bec91b33" "e0e1a92c23f643b5885e5c67815a9fdef2b9c14097cc02fc94b024880bc37684" "0ad5a61e6ee6d2e7f884c0da7a6f437a4c84547514b509bdffd06757a8fc751f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "32840b5ff3c59a31f0845602a26e9a47c27d48bfed86b4a09cdbaf3a25167cf4" "b8b5c353e73470377f2111fddb0ea652d35ce98e0a27a5c592d2ab9f66855106" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "28f5215cdff37644715c97c3ff8da3da4f8f32dd2057aa94fee0affe04557e3a" "671b540df2dc589dc3b427611c579c4827d5fde7b9410eec449b5e1438356f0b" "968c8cf5763708bb86a3f82bb0f8b8d2fe885e693ac8644268738ac2584da292" "8d6fb24169d94df45422617a1dfabf15ca42a97d594d28b3584dc6db711e0e0b" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "690585947abcb3fc7f52e6683b8375f00eef7ea55d028dcf81c9f5accb4dffe5" "3fd36152f5be7e701856c3d817356f78a4b1f4aefbbe8bbdd1ecbfa557b50006" "885ef8634f55df1fa067838330e3aa24d97be9b48c30eadd533fde4972543b55" "ed5af4af1d148dc4e0e79e4215c85e7ed21488d63303ddde27880ea91112b07e" "43099c0a941749cf91c1de52d12ca21dd4e1621b032f8e4bd2756bf544401d95" "d310ded8466cb0e56894e2d4658896ddcbf63704a376ce705e8badfe8d35e3a1" "a32b5bab23d21aa36736861d9c71c71d976d034451a1f4b6ac027da0a71b1358" "8ada1f0bcfc2d8662b74fb21bd1830eaacb5d29e3c99a5ea7fd7a417b7a9b708" "ef43b291f7e96826d3d9bae61434a93020d0f529d609bc8be5b331980e8448d7" "a31c86c0a9ba5d06480b02bb912ae58753e09f13edeb07af8927d67c3bb94d68" "758da0cfc4ecb8447acb866fb3988f4a41cf2b8f9ca28de9b21d9a68ae61b181" "42ac06835f95bc0a734c21c61aeca4286ddd881793364b4e9bc2e7bb8b6cf848" "efa048d8a9f9a8340a2f6382f3b8b8f4549cba38aa226803ff5b6a9b3a2d5f4b" "72407995e2f9932fda3347e44e8c3f29879c5ed88da71f06ba4887b0596959a4" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" "09233dff5af535c4ba3ccabc4c9267bb7bf1131cccbfab5db65e96103c7aa023" "f04122bbc305a202967fa1838e20ff741455307c2ae80a26035fbf5d637e325f" "e35ef4f72931a774769da2b0c863e11d94e60a9ad97fb9734e8b28c7ee40f49b" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "8f7e1668dd3a097964e6016c26d36822ab2e48fc3e9a3a2e2634224a5ca728c8" "1e194b1010c026b1401146e24a85e4b7c545276845fc38b8c4b371c8338172ad" "dcafe6c794d0aeec0bd898ebedeb57ef3fef9b24232d33e779a268e4be8bcda7" "ee6081af57dd389d9c94be45d49cf75d7d737c4a78970325165c7d8cb6eb9e34" "ad9fc392386f4859d28fe4ef3803585b51557838dbc072762117adad37e83585" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "7a9f392481b6e2fb027ab9d8053ab36c0f23bf5cc1271206982339370d894c74" "f290b76b7f95771d96f0ac1edae2f2d04af9e42acf83784fd4389417a71efe2f" "3dde22d0bbfe2a396bf80790ed8dd95131e69148fb931ffc1e529cc7448db299" "5ae4d52977a13212da7ec2f6932b6449f7d7c07b3cb01085f018c0d4a7f38cec" "6e92ca53a22d9b0577ad0b16e07e2e020c8b621197e39fec454048e51b7954cb" "3b2f04ff940a6273822e9f1c6b72b2e6e2b05e9c24571421a237086bc2ebe67e" "b03677b19a3fe224ddd28a1861021611139bcb13bd9f3b307b9665bbf9f7b368" "bf21256aec163a65b30bf803c8b7d26aac71616a2b4af3e754d4a2250629e02c" "2283e0e235d6f00b717ccd7b1f22aa29ce042f0f845936a221012566a810773d" "aab3160500dd6949d3cdaea37f285e7b40758aaeff3ff1bf174ed1c13719fe76" "09820052b8d8261fb71cb537ac63fdad1def22af777029911e52654f724d9884" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "450b29ed22abeeac279b7eeea592f4eea810105737716fc29807e1684e729c55" "28b17dbb4ff2013db0f007a35e06653ad386a607341f5d72e69ee91e8bbcb96c" "5a00b47220283c8686c133777ccde73be5c282958d8f6d663c1355fae602dbe1" "f48b43277382ee56a9e3c5d08b71c3639f401f6338da00039dcac3c21c0811b5" "5dfacaf380068d9ed06e0872a066a305ab6a1217f25c3457b640e76c98ae20e6" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "427234e4b45350b4159575f1ac72860c32dce79bb57a29a196b9cfb9dd3554d9" "7d090d4b15e530c28a35e084700aca464291264aad1fa2f2b6bba33dd2e92fd5" "22b0cbd7a141e0b366a536494cc5d4e30541d9b6140f9cced3f8978082719e6d" "94d66281c0398118afd3fdb921d8b813401a36748ce4541e7ad6b1533a557a9f" "337047491f7db019df2ba54483408d7d7faea0bda61e4c4f5e8cf2f4e3264478" "b2d24e5bdf57ccae593d73e44b7bb4e03aa2ba7be595694f64a66b8e8d233952" "66bd7fc2ed32703a332d05f5d2af5c30c12ff4e729d77d8271b91d6f6f7e15fc" "e57e7b19da7b4cd0e5512d5e9bc20d31c9cf50112c462de15a76bce0ea3c5ef5" "62b86b142b243071b5adb4d48a0ab89aefd3cf79ee3adc0bb297ea873b36d23f" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "d8460b6d700cbde6c5d2621790aee617f917fbe958f11a66a1bc885e3d706e40" "8f950ccbeef5ff75df54a1293c4d876fe6a932b3983e8427d776017d4745e4aa" "8d584fef1225d72bfd32d7677ac7f281208140a2535ef0e9f46f0e76343f8aca" "364a5e1aecdd0d24b70089050368851ea5ee593dc8cc6fb58cff1b8cfe88a264" "605080e40891cc991f53d3d9c79b427d18497d973a44fd12a86d2360429a6a3d" "f0283ec7a2385af8ea0fbb1466f340bbd0b2cededa60831394ec94e98096e1a8" "9b2a5e8d227bafc434a533882c1c2a40efa94024834dae53257aae56e59ff5c1" "c9cdbcbe046dcbc205b1b8ba3055ee62287a3663908a38e6e66cd7b27e2ae3b0" "d24b72ed2b0c93c0d1f816b905d31c4893d06f6fba822c2714dee19d4ff06dea" "436dd3eb5ff5be80d2db88494b340fcf34dc70a715d19c5aa7b794b763ff0321" "65680dc32f80a37717ce156071a023c2566859bf05225f929e52655f51d80e2d" "f220c05492910a305f5d26414ad82bf25a321c35aa05b1565be12f253579dec6" "c47a1363c5ab214f0f5e2c698226d048639c07709875759fd02f3459265a5aff" "a30c29c5cb75a66378d473d03b264b0bc1ee445b9c3edd276385ffec0a685d3f" "124e34f6ea0bc8a50d3769b9f251f99947d6b4d9422c6d85dc4bcc9c2e51b39c" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "2b7f47c9e1378440de6a5773a586d251ebc8436f9e89a0a59d89b76bfa15af54" "61d1a82d5eaafffbdd3cab1ac843da873304d1f05f66ab5a981f833a3aec3fc0" "30f861ee9dc270afc2a9962c05e02d600c998905433c8b9211dc2b33caa97c51" "73b835431bdbc4e83a3b176a38ebb740fbac78aa2635e1d4827b3c8211e0bc99" "635518bf81b80533e3ee3a76d55f992494ea7bf3018bf58cd3d44f100d66fa8e" "41b995882dc29bc318669ffbdf9489c3ff18cda49e55bae832ae792c0dc2f0e2" "46d144d6dd1f81a3f15da043758d6897e110597ee37e1c32bd846cc6353736de" "bf42c68919c09268cb40934a66bc75c785001f3872ab5ad85c74988e60809b29" "eacfc96fbe418c017f4a00fdde5d5029db8e2800a46251eb2174484fa431917e" "3abfa7be20483ace65a2dc36b5d8de341528bcafb5cafcb6fbb8fcba2a51465c" "a3f42f216305516caab36369dfe1fba8e14358423f6faf1d403d46a3515cd842" "3ad55e40af9a652de541140ff50d043b7a8c8a3e73e2a649eb808ba077e75792" "465be5317c7d95a84e376e095c21242f4f2ad75692ed806dcbb6fe27078260f1" "0387cbee7d97362282bb951306113eab92bee32150bfcfc8edae17e8e876aec5" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "817ce7168851955b2d67a9dfb2f4bb283504e3be87b17932bd8a3ee4b43cfeb1" "fa29856e364e2b46254503f913637ef6561faadae62668609cc671ecfcf1c3d2" "b8f561a188a77e450ab8a060128244c81dea206f15c1152a6899423dd607b327" "9562e9eb5fd01677ac6b76b66f9a81338991fa9d270270802eeae5232c8da0a6" "04643edb183240f680d5465cf9e9ac3037086f701df09ce5d9183e6c69e73a7e" "61a83dbf3d3722d70abee8fb6dbc3566766ff86c098c2a925f2ccfd4d5b3a756" "b1e54397de2c207e550dc3a090844c4b52d1a2c4a48a17163cce577b09c28236" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "ac191d0ff71167c4b07d38eb2480eeba3eead12c0c60a7dce150627722c79e62" "05751d5e0b52ad5d629403ab374742d6442d1ad32774822f3dccbfc812640758" "e3e2db3b5acd2028f7f83581f9609e1e7369df20414ab53c9b2161e2eca08675" "495adf3bee5eac6c5fbad743be36ea86a9d33d3da16b1c0643a6ec2742fda496" "466ae54a7b157ad02fd91da72b7871bccfb9bac98fdab95cf7a0d405c8572bd0" "830a42aead6991df6cbd626faa73ba3fe5513944c6bb63cdc1b57e003424f0f2" "a2e0816c1a4bea13ac9c7b7c84f22408e1ffe23cfef4c6c75a71e3dafdc9343b" "65510ff6bb3cddeb9fcdc61770abcb8cbf8a15185a0b268f77c6b515650a625b" "76b9b3780c4844712e4a3ab05b8669eecd56a3864aae29e54005ffc68c24414c" "fa189fcf5074d4964f0a53f58d17c7e360bb8f879bd968ec4a56dc36b0013d29" "383806d341087214fd44864170161c6bf34a41e866f501d1be51883e08cb674b" "a68fa33e66a883ce1a5698bc6ff355b445c87da1867fdb68b9a7325ee6ea3507" "ff2f110c788eae35007b9c2274362c5f5a824bf363be4daddad60c717075870e" "88b663861db4767f7881e5ecff9bb46d65161a20e40585c8128e8bed8747dae5" "050beead9159996a613ba4bc734de8b13b882f1c6596d1dffa4f51d096662cf6" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "f89e21c3aef10d2825f2f079962c2237cd9a45f4dc1958091be8a6f5b69bb70c" "e7091acbcd79be17a7f205049073cd95cb61f56c085b7a9e22b9c72535700acb" "978bd4603630ecb1f01793af60beb52cb44734fc14b95c62e7b1a05f89b6c811" "3341f6db5ac17e4174f7488c40676e7f0464f1e88519a59303dc7e7774245bbf" "8874901e0011a7b07e546b65be1726c4cc3f35cf1a60f8805e6cb5bb59ba305c" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "d0ff5ea54497471567ed15eb7279c37aef3465713fb97a50d46d95fe11ab4739" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "d293542c9d4be8a9e9ec8afd6938c7304ac3d0d39110344908706614ed5861c9" "6f3060ac8300275c990116794e1ba897b6a8af97c51a0cb226a98759752cddcf" "05c6a74b8254021aa039f8694fd9e35cdb077bba76c15bd9d3f303d99abef737" "65f7173faa84a97044d743d4bab115a6ab52bc6e6dc47612e31e4dbc39ebb1ae" "1f4e6cf4cb3bdba8afdb9244e037698238080eeecb209084602f7d717225f102" "3c67d690c2d801885c06f00c99c7a12c3490ce3b655be3fdc8ecc4fe0ff55706" "f27f1424a32e7c3f7d6ddfd47c8211ddbd0cadbb7d37ad7acdae195f1ecef8ad" "3555946b490880a78bdfc343fd596e29f9aa9f035b2e73bb11d0bc9f85facbe7" "764777857ef24b4ef1041be725960172ac40964b9f23a75894a578759ba6652f" "3bedd09a2afee0d1f8d52892f740cd86f665ab1291fcdefb120963a0fa9b18d7" "6c82f87e3101ca5f1e8b8bc2bc2fe4d35e8428aab9c71de8a70d243b52c3a71a" "f3b20ee84cf170fbf7d91c9f64035f2663fb16de27ca0381006f662b5ff86d97" "3a491fc2a06262c96fa151934acb829467c194a8b0fb84fd380ccb11cd951bbe" "a5a1e3cd5f790846f4eec5fcff52935e5ef6d713a0f9342fef12eccfd9e9eff0" "bc078224bd6b94486f3437265699cb1e4f5e3b91a64f8e65ada8d7ec13fa3f52" "5195dfc4aa4e8ff66248b9ba08983da04aff1d82e680fb9e008091fdb39d7c76" "fe7fac40e6dac10f982d2222dffae53776dfa0518f1eb5333ffc700971672006" "b40cca5bcb41103a07637a09d0697234a3cd8047e154c5b96c771ce4f5b03435" "c377a5f3548df908d58364ec7a0ee401ee7235e5e475c86952dc8ed7c4345d8e" "8958b8f507420410a5eb066839587cf8b9492df4c6eef2e4dfb1407260582c19" "86b5f552577fab41d24f72cb5458bf9c9c87af65e89159c50c2b8cfb20bb3087" "79243bbd9c07f2baf551c2038009afc866da65fb8073a2efce3a469efc0c1bc5" "1f3269f865292502f3dd577c3412110264ef3b89d8688aa5d649b4f496c780dc" "ca3bf8a7c831776c77d09ded89f2f0993dbdd9cb0765d8db061d1ebff806f41c" "3c708b84612872e720796ea1b069cf3c8b3e909a2e1da04131f40e307605b7f9" "1766f1b99449dc5e451929dd24735fbfa09e54b287138c3ea37d1a0a8b001a1e" "5ce9c2d2ea2d789a7e8be2a095b8bc7db2e3b985f38c556439c358298827261c" "752b605b3db4d76d7d8538bbc6fe8828f6d92a720c0ea334b4e01cea44d4b7a9" "c9d00d43bd5ad4eb7fa4c0e865b666216dfac4584eede68fbd20d7582013a703" "d971315c813b0269a86e7c5e73858070063016d9585492bd8d0f27704d50fee7" "305e0450f661c46e9584b5649c962ff3b059b0a6eafdec24854f294f3d57043f" "9bf2c3f41564c39083e717a57a4499817aa339ed59ef1812e463b7680f282593" "1cf3f29294c5a3509b7eb3ff9e96f8e8db9d2d08322620a04d862e40dc201fe2" "ad9b960b3cf80ced640ea8277ec1143be76f7f70d9a062d9a882333d39d2d2f6" "24cb1b9c182198f52df7cebf378ee9ecca93a2daeb9a90049a2f1f556119c742" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "7bc53c2f13ad0de4f1df240fde8fe3d5f11989944c69f9e02f2bd3da9ebbdcd9" "d0c0ee5c968cfa93f3fbbe6bbf339812078d7b1e74256c2715e88f0e1ad9297b" "5aae812896b7898db723645a257bf72ee91381af7200a25c1ad7a15a59c3ef5a" "d6d8a574d826c260b23c487443cc0a904d00db791cf948777a559f1c2c05fecd" "7df1ccf73c0e12f97a91aaf5fed6a7594b154137190f4ab3232b3cbc42bc9052" "f61972772958e166cda8aaf0eba700aad4faa0b4101cee319e894e7a747645c9" "2e60db7f24913de7cea9d719dc25fcf6b45682bef4693e35aec88aed3da1443e" "46d6461ab5a230038150a86ec0445c8d35722b4a00af539aefb78394bfede185" "a18dd0a954ac63a80e62c8cb1b550ffcf5d8461189c7c672555faadf2facfcf3" "ea0c5df0f067d2e3c0f048c1f8795af7b873f5014837feb0a7c8317f34417b04" "e4eaeb23c81fd6c6b1796b823dbec0129d828e13da89a222901a758348db57fd" "30d00875497336895044c85527e72453e1cf845d7315ad1fa9614078ae24591f" "65e05a8630f98308e8e804d3bbc0232b02fe2e8d24c1db358479a85f3356198d" "50ceca952b37826e860867d939f879921fac3f2032d8767d646dd4139564c68a" "e5a32add82d288d27323f9cbb9f78e3da3949bdc6283073cb98ae1dc712b6b71" "70d281d6b4f0fc83f66d58ed418a2bf7e9ed139f98224a73967cc8a98b738c58" "c822d91ff777d685f67af11ec1e4b1f70ac0493e177310a0d7d93880b0c1f0b3" "1a093e45e4c3e86fa5ad1f8003660e7cda4d961cd5d377cee3fee2dad2faf19b" "88d556f828e4ec17ac074077ef9dcaa36a59dccbaa6f2de553d6528b4df79cbd" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "5f7044d9fc9c9c9d56508ac8217483c8358a191599448859640ce80be92acbd6" "c68e02f1604f60b252ae16d75e785c8b7475e5a52d46abe6aca9992e9538c9e7" "1c1e6b2640daffcd23b1f7dd5385ca8484a060aec901b677d0ec0cf2927f7cde" "ab98e6147e144f301f31e87c70103f5a6c8bb88f3ca1eb3b8abcebaaf0e551cd" "75d4ccc5e912b93f722e57cca3ca1a15e079032cd69fd9bc67268b4c85639663" "e8529b3c5ecf27ee61ee946714998a35d07f1c5c3f778fe26b324fdeb95f0e4a" "8c5ffc9848db0f9ad4e296fa3cba7f6ea3b0e4e00e8981a59592c99d21f99471" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "ade6282873d17f6d58d9dd47bd975c5709dc0b66602db628a3183ee6fc1dd87b" "15fa54dffe7ef4c91033739a8d2eba0fb897337dffe1f98b0629978183690c42" "e40e5c86f273959e714cdbd19704a714c1d88e5e7c8a264b9709a876b0894b50" "47d2a01f2cbd853ccd1eddcb0e9e4fdfdabcc97ddad1d1a5218304294889f731" "bcb5c86c0e6576d1d6bba9bd2a55cd5c20a57d307ed13bf4ed0e86ed944e33df" "ce0b97888d66d8183309eaa66f1f4f998627daed381d10a2fd5e8e778f302a8e" "281e88e0dfab4980a157331b368fb2e5eba315c38f38099d2d9153980a8047ba" "60cd56eea72efa55d37be24b78f35dd3e9528ddeeccd6248afd8ae34e5a49c55" "07b2da5edbbeea465294fbda67df3cd00eb62aba45e4447bc60717a094d0c431" "bf8d07f24b40cb71bb2ffb56b2df537eda5101cb6c4322ba1741e29290cc260b" "885c87a0f8e1cff0eb20eb93a495d307f8c70e13a9f4fa238d2897da1081107b" "39211540c554d4d11d505a96c6baa04d43b03c04c8c3bf55f6409192936bd754" "6278f4223fd725ef15019bbaa44abf57fa0a6e5601941c31893c36e6b4ea2435" "218048771303933e49cf65c8333b7a44bd7f140d7c53c2ec311edba880fa0280" "93451314424e9fac1473a27800b563c7b166b4f8c91400384ab5b994c846318d" "63c7c4621325276cc4c4324a251a803dcc76df971be7ca1d5afb7e7c785d45b7" "533db34ca6dc89daa580fa75e2943d537d6dfab495a9c823123ce52f9cc5758d" "d286bc1bdf07b86bdb0dc3075d1e55233023b8ee218b47c74ad404d5dd3420e3" "be60eae5c0891fd7e2a12ebac78b767f6f8e10e31f36167ae22d6a019d539494" "60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "8951592dc830d55c01d1d934f90b852037bd210ef16b06e12b4f91f582fd4227" "59946e8f0cb8dfe511443f881282b68b3c387b4e1afc21d73541b726ed472bf4" "84c86f8899d2e2883310cd1708ec6a1ac873f3cc9a3e83719860871a3f59a92f" "32fbec56777e19aba35fa47ccf2f6f1d8355d00efba5c5389c85fb4c36d5ece8" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "3dd173744ae0990dd72094caef06c0b9176b3d98f0ee5d822d6a7e487c88d548" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "211bb9b24001d066a646809727efb9c9a2665c270c753aa125bace5e899cb523" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "998e84b018da1d7f887f39d71ff7222d68f08d694fe0a6978652fb5a447bdcd2" "967c58175840fcea30b56f2a5a326b232d4939393bed59339d21e46cf4798ecf" "1760322f987b57884e38f4076ac586c27566a1d7ed421b67843c8c98a1501e3a" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "f38dd27d6462c0dac285aa95ae28aeb7df7e545f8930688c18960aeaf4e807ed" "38c4fb6c8b2625f6307f3dde763d5c61d774d854ecee9c5eb9c5433350bc0bef" "085b401decc10018d8ed2572f65c5ba96864486062c0a2391372223294f89460" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "344ff60900acf388116822a0540b34699fc575cf29a5c9764453d045cc42a476" "ca2d69f5dd853dbf6fbcf5d0f1759ec357fda19c481915431015417ec9c1fbd8" "32ff89088ee3518fc03954b09ecca3614fdc51aa5108dfb3b3cba68083b701f6" "935e766f12c5f320c360340c8d9bc1726be9f8eb01ddeab312895487e50e5835" "cfd71d55f448690641d6e8ca6438ab696bcaff3296905f95d91d4990166863d5" "36afe64261e1de73fcfadedf154e4bc2c9ec1969bde0c21798d31366897bc4d2" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "5727ad01be0a0d371f6e26c72f2ef2bafdc483063de26c88eaceea0674deb3d9" "723d8e038750a51aa9d6f1000a6f5047f343a10291a07dfb30c8f35fa9bfe8ec" "9d0abcc1835bfb11ba0700f4a40378bab559523ad17cfa0765fde7b810a762bb" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" "fde28e997568821defbad5f24c48e2f3db3175e0b6b180a8c1a02f5c5555aa2c" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" "baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" "7acc0466fce1bc967ce1561c8c4fdcbf4358b4ae692577562a3ed747c109f9d7" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#000000" :underline
          (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
                         (:color "#c0c0c0"))
     (implicitParams :underline
                     (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000"))))
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)))
 '(evil-insert-state-cursor (quote ("#D50000" bar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#202325")
 '(gnus-logo-colors (quote ("#1ec1c4" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315")))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors (quote (("#F8BBD0" . 0) ("#FAFAFA" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-sexp-background-color "#1c1f26")
 '(linum-format " %3i ")
 '(magit-diff-use-overlays nil)
 '(magit-dispatch-arguments nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (debian-changelog-mode zerodark-theme zenburn-theme yoshi-theme which-key webpaste web-mode w3m visual-regexp utop use-package tuareg toxi-theme theme-looper terminal-here tao-theme tabbar-ruler suscolors-theme sunburn-theme sudo-edit sublime-themes spacemacs-theme solarized-theme snazzy-theme smartparens smart-newline smart-backspace slime silkworm-theme shackle rust-mode rpm-spec-mode reykjavik-theme rainbow-delimiters popwin polymode planet-theme pandoc-mode organic-green-theme org-tree-slide ocp-indent nubox nix-mode nimbus-theme niceify-info neotree muttrc-mode molokai-theme moe-theme modern-cpp-font-lock meson-mode matlab-mode material-theme markdown-mode majapahit-theme lua-mode liso-theme labburn-theme kooten-theme kaolin-themes jbeans-theme ivy-rich irony-eldoc iqa intellij-theme imenu-list idea-darkula-theme hamburg-theme gruvbox-theme grayscale-theme goose-theme google-translate github-modern-theme fringe-current-line forest-blue-theme folding fold-dwim flycheck-ocaml flycheck-irony flycheck-clang-analyzer flatui-dark-theme faff-theme exotica-theme epresent eink-theme dokuwiki-mode diredfl dired-sort-menu delight default-text-scale dark-mint-theme dante danneskjold-theme counsel company-web company-nixos-options company-math company-irony-c-headers company-irony company-emoji company-auctex color-theme-sanityinc-tomorrow cmake-ide cmake-font-lock clean-buffers challenger-deep-theme boxquote bison-mode badwolf-theme avk-emacs-themes auctex-latexmk apropospriate-theme anti-zenburn-theme alect-themes adoc-mode abyss-theme)))
 '(pdf-view-midnight-colors (quote ("#232333" . "#c7c7c7")))
 '(pos-tip-background-color "#ffffff")
 '(pos-tip-foreground-color "#78909C")
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(scroll-bar-mode (quote right))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#657b83" 0.2))
 '(tabbar-background-color "#ffffff")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#1f2124")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0000")
     (40 . "#ff4a52")
     (60 . "#f6aa11")
     (80 . "#f1e94b")
     (100 . "#f5f080")
     (120 . "#f6f080")
     (140 . "#41a83e")
     (160 . "#40b83e")
     (180 . "#b6d877")
     (200 . "#b7d877")
     (220 . "#b8d977")
     (240 . "#b9d977")
     (260 . "#93e0e3")
     (280 . "#72aaca")
     (300 . "#8996a8")
     (320 . "#afc4db")
     (340 . "#cfe2f2")
     (360 . "#dc8cc3"))))
 '(vc-annotate-very-old-color "#dc8cc3")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(when
      (or
       (not
        (boundp
         (quote ansi-term-color-vector)))
       (not
        (facep
         (aref ansi-term-color-vector 0)))))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

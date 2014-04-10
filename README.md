webqq-emacs
===========

This is a webqq plugin for Emacs



使用前需要的配置
======================================
1. 系统需要安装curl md5deep 软件包. Debian/Ubuntu系统请使用下面命令.

```
apt-get install curl md5deep
```

2. 将webqq-emacs.el添加到emacs的加载环境中.

3. 项目依赖于 `json.el` `url.el` `cl.el` `pp.el` `async.el`(emacs-async)

4. 为了自己调试方便, 使用了日志功能,记录了过程中的一些信息

定义了这个路径存放日志文件

```
(defconst webqq-log-file-name "~/Logs/my_emacs_log.log")
```

可以自行修改代码


5. 使用脚本`encode.sh` 来处理登录时的密码签名, 我将脚本放在了下面的文件夹中,


```
(defvar webqq-project-path "/home/savior/programming/Emacs-lisp/webQQ-emacs/resources")
```

使用者请根据具体情况进行修改(后期会处理调这个问题)



使用过程中的问题
======================================
1. 登录的时候会获取用户的好友列表和群列表,比较消耗时间, Emacs会有假死的感觉,可以通过察看日志信息判断是否依然正常.

2. 目前只可以发送和接收纯文本文件, 支持好友和群信息, (还没有支持临时会话和讨论组)
 登录时获取群信息时有可能部分(或者全部)群信息请求失败, 这样接收群消息的时候不会显示消息是谁说的, 根据`webqq-show-remain-need-to-get-group-info` 函数察看需要手动更新的群信息, 将光标移动到群的gid 编号上,调用 `webqq-reget-group-info-with-select`手懂更新群信息

3. 现在还出于玩具状态, 界面丑陋, 同时还存在网络上问题导致消息有可能发送和接收失败(就做了这么一点事情,还有可能异常, 冏 ~ ~)



介绍
============================================

启动
![](./1.png)

输入用户名和密码
![](./2.png)

群信息
![](./3.png)




感谢
============================================
感谢iQQ所做的工作, 网络协议相关的工作都是直接从该项目代码中获得的, 偶尔使用wireshark进行数据包分析, 但是iQQ省去了我大量的协议分析工作.
